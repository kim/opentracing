{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}

module OpenTracing.Zipkin.HttpReporter
    ( Env
    , newEnv
    , zipkinHttpReporter
    )
where

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Lens             hiding (Context)
import           Control.Monad            (forever, void)
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Aeson               hiding (Error)
import           Data.Aeson.Encoding
import           Data.Foldable
import qualified Data.HashSet             as HashSet
import           Data.Maybe               (catMaybes)
import           Data.Monoid
import           Data.Text                (Text)
import qualified Data.Text                as Text
import           Data.Text.Lens           (packed)
import           Data.Time.Clock.POSIX
import           Data.Word
import           GHC.Generics             (Generic)
import           GHC.Stack                (prettyCallStack)
import           Network.HTTP.Client      hiding (port)
import           OpenTracing.Class
import           OpenTracing.Log
import           OpenTracing.Span
import           OpenTracing.Tags
import           OpenTracing.Types
import           OpenTracing.Zipkin       hiding (Env, newEnv)


data Endpoint = Endpoint
    { serviceName :: Maybe Text
    , ipv4        :: Maybe IPv4
    , ipv6        :: Maybe IPv6
    , port        :: Maybe Port
    } deriving (Eq, Show, Generic)

instance ToJSON Endpoint where
    toEncoding Endpoint{..} = pairs . mconcat . catMaybes $
        [ pair "serviceName" . text <$> serviceName
        , pair "ipv4" . toEncoding  <$> ipv4
        , pair "ipv6" . toEncoding  <$> ipv6
        , pair "port" . toEncoding  <$> port
        ]


data Env = Env
    { envQ   :: TQueue (FinishedSpan ZipkinContext)
    , envRep :: Async () -- TODO: cancel when done
    }

newEnv :: Endpoint -> String -> Port -> IO Env
newEnv loc zhost zport = do
    q   <- newTQueueIO
    rq  <- parseRequest ("POST " <> zhost <> ":" <> show zport <> "/api/v2")
    mgr <- newManager defaultManagerSettings
    Env q <$> reporter rq mgr loc q

instance MonadIO m => MonadReport ZipkinContext (ReaderT Env m) where
    traceReport = report

zipkinHttpReporter :: Env -> Interpret (MonadReport ZipkinContext) MonadIO
zipkinHttpReporter r = Interpret $ \m -> runReaderT m r


report :: (MonadIO m, MonadReader Env m) => FinishedSpan ZipkinContext -> m ()
report s = do
    q <- asks envQ
    liftIO . atomically $ writeTQueue q s

reporter
    :: Request
    -> Manager
    -> Endpoint
    -> TQueue (FinishedSpan ZipkinContext)
    -> IO (Async ())
reporter rq mgr loc q = async . forever $ do
    batch <- atomically $ pop 100 q
    case batch of
        [] -> void . atomically $ peekTQueue q
        xs -> void $ httpLbs rq { requestBody = body xs } mgr
  where
    body = RequestBodyLBS . encodingToLazyByteString . list (spanE loc)


pop :: Int -> TQueue a -> STM [a]
pop 0 _ = pure []
pop n q = do
    v <- tryReadTQueue q
    case v of
        Nothing -> pure []
        Just v' -> (v' :) <$> pop (n-1) q


spanE :: Endpoint -> FinishedSpan ZipkinContext -> Encoding
spanE loc s = pairs $
       pair "name"           (view (spanOperation . to text) s)
    <> pair "id"             (view (spanContext . to ctxSpanID  . re _ID . to text) s)
    <> pair "traceId"        (view (spanContext . to ctxTraceID . re _ID . to text) s)
    <> maybe mempty
            (pair "parentid" . text . review _ID)
            (view (spanContext . to ctxParentSpanID) s)
    <> maybe mempty
            (pair "kind" . toEncoding)
            (view (spanTags . to (getTag SpanKindKey)) s)
    <> pair "timestamp"      (view (spanStart . to utcTimeToPOSIXSeconds . to micros . to word64) s)
    <> pair "duration"       (view (spanDuration . to micros . to word64) s)
    <> pair "debug"          (bool . HashSet.member Debug $ view (spanContext . ctxFlags) s)
    <> pair "localEndpoint"  (toEncoding loc)
    <> pair "remoteEndpoint" (view (spanTags . to remoteEndpoint) s)
    <> pair "annotations"    (list logRecE $ view spanLogs s)
    <> pair "tags"           (toEncoding $ view spanTags s)
    -- nb. references are lost, perhaps we should stick them into annotations?

remoteEndpoint :: Tags -> Encoding
remoteEndpoint ts = pairs . mconcat . catMaybes $
    [ pair PeerServiceKey . toEncoding <$> getTag PeerServiceKey ts
    , pair PeerIPv4Key    . toEncoding <$> getTag PeerIPv4Key    ts
    , pair PeerIPv6Key    . toEncoding <$> getTag PeerIPv6Key    ts
    , pair PeerPortKey    . toEncoding <$> getTag PeerPortKey    ts
    ]

logRecE :: LogRecord -> Encoding
logRecE r = pairs $
       pair "timestamp" (view (logTime . to utcTimeToPOSIXSeconds . to micros . to word64) r)
    <> pair "value"     (text . Text.intercalate " " . toList . fmap field $ view logFields r)
  where
    field f = logFieldLabel f <> "=" <> case f of
        Event      x -> x
        Message    x -> x
        Stack      x -> view packed . prettyCallStack $ x
        ErrKind    x -> x
        ErrObj     x -> view packed (show x)
        LogField _ x -> view packed (show x)

micros :: POSIXTime -> Word64
micros = round . (1000000 *)
