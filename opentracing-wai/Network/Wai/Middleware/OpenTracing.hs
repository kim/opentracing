{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}

module Network.Wai.Middleware.OpenTracing
    ( TracedApplication
    , OperationName
    , opentracing
    , withOperationName
    , defaultOperationName
    )
where

import           Control.Lens            (over, set, view)
import           Data.Maybe
import           Data.Semigroup
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           Data.Text.Encoding      (decodeUtf8)
import           Network.Wai
import           OpenTracing
import qualified OpenTracing.Propagation as Propagation
import qualified OpenTracing.Tracer      as Tracer
import           Prelude                 hiding (span)


-- | A 'TracedApplication' is a WAI 'Application' with an 'ActiveSpan`.
--
-- Expanded:
--
-- @
-- type TracedApplication =
--     ActiveSpan -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
-- @
type TracedApplication = ActiveSpan -> Application

-- | The operation name is, basically, the name of the span.
--
-- This is typically determined from the request in some way, see
-- 'defaultOperationName'.
--
-- @since 0.2.0
type OperationName = Request -> Text

-- | Middleware to enable tracing for a WAI application.
--
-- This uses the 'defaultOperationName'.
opentracing
    :: HasCarrier Headers p
    => Tracer
    -> Propagation        p
    -> TracedApplication
    -> Application
opentracing t p app req respond =
    withOperationName t p defaultOperationName app req respond

-- | Customise the tracing middleware with an 'OperationName'.
--
-- It is intended to import this module qualified for legibility
-- (@OpenTracing.withOperationName@).
--
-- @since 0.2.0
withOperationName
    :: HasCarrier Headers p
    => Tracer
    -> Propagation        p
    -> OperationName
    -> TracedApplication
    -> Application
withOperationName t p opname app req respond = do
    let ctx = Propagation.extract p (requestHeaders req)
    let opt = let name = opname req
                  refs = (\x -> set refPropagated x mempty)
                       . maybeToList . fmap ChildOf $ ctx
               in set spanOptSampled (view ctxSampled <$> ctx)
                . set spanOptTags
                      [ HttpMethod  (requestMethod req)
                      , HttpUrl     (decodeUtf8 url)
                      , PeerAddress (Text.pack (show (remoteHost req))) -- not so great
                      , SpanKind    RPCServer
                      ]
                $ spanOpts name refs

    Tracer.traced_ t opt $ \span -> app span req $ \res -> do
        modifyActiveSpan span $
            over spanTags (setTag (HttpStatusCode (responseStatus res)))
        respond res
  where
    url = "http" <> if isSecure req then "s" else mempty <> "://"
       <> fromMaybe "localhost" (requestHeaderHost req)
       <> rawPathInfo req <> rawQueryString req

-- | The default 'OperationName' is the @pathInfo@ of the request.
--
-- @since 0.2.0
defaultOperationName :: OperationName
defaultOperationName req = Text.cons '/' (Text.intercalate "/" (pathInfo req))
