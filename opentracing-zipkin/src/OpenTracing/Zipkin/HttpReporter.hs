{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TemplateHaskell   #-}

module OpenTracing.Zipkin.HttpReporter
    ( ZipkinOptions
    , zipkinOptions
    , zoManager
    , zoApiVersion
    , zoLocalEndpoint
    , zoAddr
    , zoLogfmt
    , zoErrorLog

    , API(..)

    , defaultZipkinAddr

    , Zipkin
    , newZipkin
    , closeZipkin
    , withZipkin

    , zipkinHttpReporter

    , Endpoint(..)

    , newManager
    , defaultManagerSettings
    )
where

import Control.Lens              hiding (Context)
import Control.Monad             (unless)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Aeson                hiding (Error)
import Data.Aeson.Encoding
import Data.ByteString.Builder
import Data.Maybe                (catMaybes)
import Data.Monoid
import Data.Text.Lazy.Encoding   (decodeUtf8)
import Network.HTTP.Client       hiding (port)
import Network.HTTP.Types
import OpenTracing.Log
import OpenTracing.Reporting
import OpenTracing.Span
import OpenTracing.Tags
import OpenTracing.Time
import OpenTracing.Types
import OpenTracing.Zipkin.Thrift
import OpenTracing.Zipkin.Types


data API = V1 | V2

newtype Zipkin = Zipkin { fromZipkin :: BatchEnv }

data ZipkinOptions = ZipkinOptions
    { _zoManager       :: Manager
    , _zoApiVersion    :: API
    , _zoLocalEndpoint :: Endpoint
    , _zoAddr          :: Addr 'HTTP
    , _zoLogfmt        :: forall t. Foldable t => t LogField -> Builder -- == LogFieldsFormatter
    , _zoErrorLog      :: Builder -> IO ()
    }

makeLenses ''ZipkinOptions

zipkinOptions :: Manager -> API -> Endpoint -> ZipkinOptions
zipkinOptions mgr api loc = ZipkinOptions
    { _zoManager       = mgr
    , _zoApiVersion    = api
    , _zoLocalEndpoint = loc
    , _zoAddr          = defaultZipkinAddr
    , _zoLogfmt        = jsonMap
    , _zoErrorLog      = defaultErrorLog
    }

defaultZipkinAddr :: Addr 'HTTP
defaultZipkinAddr = HTTPAddr "127.0.0.1" 9411 False

newZipkin :: ZipkinOptions -> IO Zipkin
newZipkin opts@ZipkinOptions{_zoErrorLog=errlog,_zoApiVersion} = do
    rq <- mkReq
    fmap Zipkin
        . newBatchEnv
        . set boptErrorLog errlog . batchOptions
        $ reporter opts rq
  where
    mkReq = do
        let rqBase = "POST http://"
                   <> view (zoAddr . addrHostName) opts
                   <> ":"
                   <> show (view (zoAddr . addrPort) opts)
        case _zoApiVersion of
            V1 -> do
                rq <- parseRequest $ rqBase <> "/api/v1/spans"
                return rq
                    { requestHeaders = [(hContentType, "application/x-thrift")]
                    , secure         = view (zoAddr . addrSecure) opts
                    }
            V2 -> do
                rq <- parseRequest $ rqBase <> "/api/v2/spans"
                return rq
                    { requestHeaders = [(hContentType, "application/json")]
                    , secure         = view (zoAddr . addrSecure) opts
                    }

closeZipkin :: Zipkin -> IO ()
closeZipkin = closeBatchEnv . fromZipkin

withZipkin
    :: ( MonadIO   m
       , MonadMask m
       )
    => ZipkinOptions
    -> (Zipkin -> m a)
    -> m a
withZipkin opts = bracket (liftIO $ newZipkin opts) (liftIO . closeZipkin)


zipkinHttpReporter :: MonadIO m => Zipkin -> FinishedSpan -> m ()
zipkinHttpReporter = batchReporter . fromZipkin

reporter :: ZipkinOptions -> Request -> [FinishedSpan] -> IO ()
reporter ZipkinOptions{..} rq spans = do
    rs <- responseStatus <$> httpLbs rq { requestBody = body } _zoManager
    unless (statusIsSuccessful rs) $
        _zoErrorLog $ shortByteString "Error from Zipkin server: "
                    <> intDec (statusCode rs)
                    <> char8 '\n'
  where
    body = RequestBodyLBS $ case _zoApiVersion of
        V1 -> thriftEncodeSpans $
                  map (toThriftSpan _zoLocalEndpoint _zoLogfmt) spans
        V2 -> encodingToLazyByteString $
                  list (spanE _zoLocalEndpoint _zoLogfmt) spans


spanE :: Endpoint -> LogFieldsFormatter -> FinishedSpan -> Encoding
spanE loc logfmt s = pairs $
       pair "name"           (view (spanOperation . to text) s)
    <> pair "id"             (view (spanContext . to ctxSpanID  . hexText . to text) s)
    <> pair "traceId"        (view (spanContext . to ctxTraceID . hexText . to text) s)
    <> maybe mempty
            (pair "parentId" . text . view hexText)
            (view (spanContext . to ctxParentSpanID) s)
    <> maybe mempty
            (pair "kind" . toEncoding)
            (view (spanTags . to (getTag SpanKindKey)) s)
    <> pair "timestamp"      (view (spanStart . to microsE) s)
    <> pair "duration"       (view (spanDuration . to microsE) s)
    <> pair "debug"          (bool False)
    <> pair "localEndpoint"  (toEncoding loc)
    <> maybe mempty
             (pair "remoteEndpoint")
             (view (spanTags . to remoteEndpoint) s)
    <> pair "annotations"    (list (logRecE logfmt) $ view spanLogs s)
    <> pair "tags"           (toEncoding $ view spanTags s)
    -- nb. references are lost, perhaps we should stick them into annotations?

remoteEndpoint :: Tags -> Maybe Encoding
remoteEndpoint ts = case fields of
    [] -> Nothing
    xs -> Just . pairs $ mconcat xs
  where
    fields = catMaybes
        [ pair PeerServiceKey . toEncoding <$> getTag PeerServiceKey ts
        , pair PeerIPv4Key    . toEncoding <$> getTag PeerIPv4Key    ts
        , pair PeerIPv6Key    . toEncoding <$> getTag PeerIPv6Key    ts
        , pair PeerPortKey    . toEncoding <$> getTag PeerPortKey    ts
        ]

logRecE :: LogFieldsFormatter -> LogRecord -> Encoding
logRecE logfmt r = pairs $
       pair "timestamp" (view (logTime . to microsE) r)
    <> pair "value"     (lazyText . decodeUtf8 . toLazyByteString . logfmt $ view logFields r)

microsE :: AsMicros a => a -> Encoding
microsE = word64 . micros
