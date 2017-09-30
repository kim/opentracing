{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Network.Wai.Middleware.OpenTracing where

import           Control.Lens           (view)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Maybe             (fromMaybe)
import           Data.Semigroup
import qualified Data.Text              as Text
import           Data.Text.Encoding     (decodeUtf8)
import           Network.Wai
import           OpenTracing
import           Prelude                hiding (span)


type TracedApplication ctx = Span ctx -> Application

opentracing
    :: ( HasSampled ctx
       , AsCarrier  HttpHeaders ctx
       )
    => Tracing ctx MonadIO
    -> TracedApplication ctx
    -> Application
opentracing tracing app = \req respond -> do
    let ctx = traceExtract (HttpHeaders (requestHeaders req))
        opt = SpanOpts
            { spanOptOperation = Text.intercalate "/" (pathInfo req)
            , spanOptRefs      = maybe mempty (\x -> [ChildOf x]) ctx
            , spanOptTags      =
                [ HttpMethod  (requestMethod req)
                , HttpUrl     (decodeUtf8 (url req))
                , PeerAddress (Text.pack (show (remoteHost req))) -- not so great
                , SpanKind    RPCServer
                ]
            , spanOptSampled   = view ctxSampled <$> ctx
            }
    traced' tracing opt (\span -> app span req respond)
  where
    url req = "http" <> if isSecure req then "s" else mempty <> "://"
           <> fromMaybe "localhost" (requestHeaderHost req)
           <> rawPathInfo req <> rawQueryString req
