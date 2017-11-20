{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}

module OpenTracing.Zipkin.HttpReporter
    ( API(..)
    , Env
    , newEnv
    , closeEnv
    , withEnv

    , ZipkinAddr(..)
    , defaultZipkinAddr

    , zipkinHttpReporter

    , Endpoint(..)
    )
where

import Control.Lens              hiding (Context)
import Control.Monad             (void)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Aeson                hiding (Error)
import Data.Aeson.Encoding
import Data.ByteString.Builder   (toLazyByteString)
import Data.Maybe                (catMaybes)
import Data.Monoid
import Data.Text.Lazy.Encoding   (decodeUtf8)
import Network.HTTP.Client       hiding (port)
import Network.HTTP.Types        (hContentType)
import OpenTracing.Log
import OpenTracing.Reporting
import OpenTracing.Span
import OpenTracing.Tags
import OpenTracing.Time
import OpenTracing.Types
import OpenTracing.Zipkin.Thrift
import OpenTracing.Zipkin.Types


data API = V1 | V2

type Env = BatchEnv

data ZipkinAddr = ZipkinAddr
    { zipkinHost   :: String
    , zipkinPort   :: Port
    , zipkinSecure :: Bool
    }

defaultZipkinAddr :: ZipkinAddr
defaultZipkinAddr = ZipkinAddr
    { zipkinHost   = "127.0.0.1"
    , zipkinPort   = 9411
    , zipkinSecure = False
    }


newEnv :: Manager -> API -> Endpoint -> ZipkinAddr -> LogFieldsFormatter -> IO Env
newEnv mgr api loc ZipkinAddr{..} logfmt = do
    rq  <- mkReq
    newBatchEnv 100 $ reporter api rq mgr loc logfmt
  where
    mkReq = do
        let rqBase = "POST http://" <> zipkinHost <> ":" <> show zipkinPort
        case api of
            V1 -> do
                rq <- parseRequest $ rqBase <> "/api/v1/spans"
                return rq
                    { requestHeaders = [(hContentType, "application/x-thrift")]
                    , secure         = zipkinSecure
                    }
            V2 -> do
                rq <- parseRequest $ rqBase <> "/api/v2/spans"
                return rq
                    { requestHeaders = [(hContentType, "application/json")]
                    , secure         = zipkinSecure
                    }

closeEnv :: Env -> IO ()
closeEnv = closeBatchEnv

withEnv
    :: ( MonadIO   m
       , MonadMask m
       )
    => Manager
    -> API
    -> Endpoint
    -> ZipkinAddr
    -> LogFieldsFormatter
    -> (Env -> m a)
    -> m a
withEnv mgr api loc addr logfmt =
    bracket (liftIO $ newEnv mgr api loc addr logfmt) (liftIO . closeEnv)


zipkinHttpReporter :: MonadIO m => Env -> FinishedSpan -> m ()
zipkinHttpReporter = batchReporter

reporter
    :: API
    -> Request
    -> Manager
    -> Endpoint
    -> LogFieldsFormatter
    -> [FinishedSpan]
    -> IO ()
reporter api rq mgr loc logfmt spans =
    void $ httpLbs rq { requestBody = body } mgr -- XXX: check response status
  where
    body = RequestBodyLBS $ case api of
        V1 -> thriftEncodeSpans $ map (toThriftSpan loc logfmt) spans
        V2 -> encodingToLazyByteString $ list (spanE loc logfmt) spans


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
