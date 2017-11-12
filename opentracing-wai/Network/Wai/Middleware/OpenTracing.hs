{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Network.Wai.Middleware.OpenTracing (opentracing) where

import           Control.Lens           (over, set, view)
import           Data.Maybe
import           Data.Semigroup
import qualified Data.Text              as Text
import           Data.Text.Encoding     (decodeUtf8)
import           Network.Wai
import           OpenTracing
import           Prelude                hiding (span)


type TracedApplication ctx = ActiveSpan ctx -> Application

opentracing
    :: ( HasSampled ctx
       , AsCarrier  HttpHeaders ctx ctx
       )
    => Tracing           ctx
    -> TracedApplication ctx
    -> Application
opentracing tracing app req respond = do
    let ctx = traceExtract (HttpHeaders (requestHeaders req))
    let opt = SpanOpts
            { spanOptOperation = Text.intercalate "/" (pathInfo req)
            , spanOptRefs      = (\x -> set refPropagated x mempty)
                               . maybeToList
                               . fmap ChildOf
                               $ ctx
            , spanOptSampled   = view ctxSampled <$> ctx
            , spanOptTags      =
                [ HttpMethod  (requestMethod req)
                , HttpUrl     (decodeUtf8 url)
                , PeerAddress (Text.pack (show (remoteHost req))) -- not so great
                , SpanKind    RPCServer
                ]
            }

    fmap tracedResult . traced' tracing opt $ \span -> app span req $ \res -> do
        modifyActiveSpan span $
            over spanTags (setTag (HttpStatusCode (responseStatus res)))
        respond res
  where
    url = "http" <> if isSecure req then "s" else mempty <> "://"
       <> fromMaybe "localhost" (requestHeaderHost req)
       <> rawPathInfo req <> rawQueryString req
