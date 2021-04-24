{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}

module Network.Wai.Middleware.OpenTracing
    ( TracedApplication
    , opentracing
    )
where

import           Control.Lens            (over, set, view)
import           Data.Maybe
import           Data.Semigroup
import qualified Data.Text               as Text
import           Data.Text.Encoding      (decodeUtf8)
import           Network.Wai
import           OpenTracing
import qualified OpenTracing.Propagation as Propagation
import qualified OpenTracing.Tracer      as Tracer
import           Prelude                 hiding (span)


type TracedApplication = ActiveSpan -> Application

opentracing
    :: HasCarrier Headers p
    => Tracer
    -> Propagation        p
    -> TracedApplication
    -> Application
opentracing t p app req respond = do
    let ctx = Propagation.extract p (requestHeaders req)
    let opt = let name = Text.cons '/' (Text.intercalate "/" (pathInfo req))
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
