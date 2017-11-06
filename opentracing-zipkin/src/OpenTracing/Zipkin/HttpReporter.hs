{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}

module OpenTracing.Zipkin.HttpReporter
    ( API(..)
    , Env
    , newEnv
    , closeEnv
    , withEnv

    , zipkinHttpReporter

    , Endpoint(..)
    )
where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception         (AsyncException (ThreadKilled))
import Control.Exception.Safe
import Control.Lens              hiding (Context)
import Control.Monad             (forever, void)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson                hiding (Error)
import Data.Aeson.Encoding
import Data.ByteString.Builder   (toLazyByteString)
import Data.Maybe                (catMaybes)
import Data.Monoid
import Data.Text.Lazy.Encoding   (decodeUtf8)
import Network.HTTP.Client       hiding (port)
import Network.HTTP.Types        (hContentType)
import OpenTracing.Class
import OpenTracing.Log
import OpenTracing.Span
import OpenTracing.Tags
import OpenTracing.Time
import OpenTracing.Types
import OpenTracing.Zipkin        hiding (Env, newEnv)
import OpenTracing.Zipkin.Thrift
import OpenTracing.Zipkin.Types


data API = V1 | V2

data Env = Env
    { envQ   :: TQueue (FinishedSpan ZipkinContext)
    , envRep :: Async ()
    }

-- XXX: support https
newEnv :: API -> Endpoint -> String -> Port -> LogFieldsFormatter -> IO Env
newEnv api loc zhost zport logfmt = do
    q   <- newTQueueIO
    rq  <- mkReq
    mgr <- newManager defaultManagerSettings
    Env q <$> reporter api rq mgr loc q logfmt
  where
    mkReq = do
        let rqBase = "POST http://" <> zhost <> ":" <> show zport
        case api of
            V1 -> do
                rq <- parseRequest $ rqBase <> "/api/v1/spans"
                return rq
                    { requestHeaders = [(hContentType, "application/x-thrift")]
                    }
            V2 -> do
                rq <- parseRequest $ rqBase <> "/api/v2/spans"
                return rq
                    { requestHeaders = [(hContentType, "application/json")]
                    }

closeEnv :: Env -> IO ()
closeEnv = cancel . envRep

withEnv
    :: ( MonadIO   m
       , MonadMask m
       )
    => API
    -> Endpoint
    -> String
    -> Port
    -> LogFieldsFormatter
    -> (Env -> m a)
    -> m a
withEnv api loc zhost zport logfmt
    = bracket (liftIO $ newEnv api loc zhost zport logfmt) (liftIO . closeEnv)

instance MonadIO m => MonadReport ZipkinContext (ReaderT Env m) where
    traceReport = report

zipkinHttpReporter :: Env -> Interpret (MonadReport ZipkinContext) MonadIO
zipkinHttpReporter r = Interpret $ \m -> runReaderT m r


report :: (MonadIO m, MonadReader Env m) => FinishedSpan ZipkinContext -> m ()
report s = do
    q <- asks envQ
    liftIO . atomically $ writeTQueue q s

reporter
    :: API
    -> Request
    -> Manager
    -> Endpoint
    -> TQueue (FinishedSpan ZipkinContext)
    -> LogFieldsFormatter
    -> IO (Async ())
reporter api rq mgr loc q logfmt = async . handle drain . forever $
    go (void . atomically $ peekTQueue q)
  where
    go onEmpty = do
        batch <- atomically $ pop 100 q
        case batch of
            [] -> onEmpty
            xs -> mask_ $
                    void (httpLbs rq { requestBody = body xs } mgr) -- XXX: check response status
                        `catchAny` const (return ()) -- XXX: log something

    body xs = RequestBodyLBS $ case api of
        V1 -> thriftEncodeSpans $ map (toThriftSpan loc logfmt) xs
        V2 -> encodingToLazyByteString $ list (spanE loc logfmt) xs

    drain ThreadKilled = go (return ())
    drain e            = throwM e

pop :: Int -> TQueue a -> STM [a]
pop 0 _ = pure []
pop n q = do
    v <- tryReadTQueue q
    case v of
        Nothing -> pure []
        Just v' -> (v' :) <$> pop (n-1) q


spanE :: Endpoint -> LogFieldsFormatter -> FinishedSpan ZipkinContext -> Encoding
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
    <> pair "debug"          (view (spanContext . to (hasFlag Debug) . to bool) s)
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
