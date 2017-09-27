{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

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
import           Data.Set                 (Set, lookupLT)
import           Data.Text                (Text)
import qualified Data.Text                as Text
import           Data.Text.Lens           (packed)
import           Data.Time.Clock.POSIX
import           Data.Word
import           GHC.Generics             (Generic)
import           GHC.Stack                (prettyCallStack)
import           Network.HTTP.Client      hiding (port)
import           Network.HTTP.Types       (statusCode)
import           OpenTracing.Class
import           OpenTracing.Types
import           OpenTracing.Zipkin       hiding (Env, newEnv)


data Endpoint = Endpoint
    { srv  :: Maybe Text
    , ipv4 :: Maybe IPv4
    , ipv6 :: Maybe IPv6
    , port :: Maybe Port
    } deriving (Eq, Show, Generic)

instance ToJSON Endpoint where
    toEncoding Endpoint{..} = pairs . mconcat . catMaybes $
        [ pair "serviceName" . text <$> srv
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
            (pair "kind" . text . spanKindLabel)
            (view (spanTags . to spanKind) s)
    <> pair "timestamp"      (view (spanStart . to utcTimeToPOSIXSeconds . to micros . to word64) s)
    <> pair "duration"       (view (spanDuration . to micros . to word64) s)
    <> pair "debug"          (bool . HashSet.member Debug $ view (spanContext . ctxFlags) s)
    <> pair "localEndpoint"  (toEncoding loc)
    <> pair "remoteEndpoint" (view (spanTags . to remoteEndpoint . to toEncoding) s)
    <> pair "annotations"    (list logRecE $ view spanLogs s)
    <> pair "tags"           (list tagE . toList $ view spanTags s)
    -- nb. references are lost, perhaps we should stick them into annotations?


spanKind :: Set Tag -> Maybe SpanKinds
spanKind ts = lookupLT (SomeTag mempty mempty) ts >>= \case
    SpanKind x -> pure x
    _          -> Nothing

remoteEndpoint :: Set Tag -> Endpoint
remoteEndpoint = foldl' matchTag (Endpoint Nothing Nothing Nothing Nothing)
  where
    matchTag ep (PeerService s) = ep { srv  = Just s  }
    matchTag ep (PeerIPv4   ip) = ep { ipv4 = Just ip }
    matchTag ep (PeerIPv6   ip) = ep { ipv6 = Just ip }
    matchTag ep (PeerPort    p) = ep { port = Just p  }

    matchTag ep _               = ep

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
        LogField _ x -> view packed (show x)

tagE :: Tag -> Encoding
tagE t = pairs . pair (tagLabel t) $ case t of
    Component             x -> text x
    DbInstance            x -> text x
    DbStatement           x -> text x
    DbType                x -> text x
    DbUser                x -> text x
    Error                 x -> bool x
    HttpMethod            x -> string . show $ x
    HttpStatusCode        x -> int . statusCode $ x
    HttpUrl               x -> text x
    MessageBusDestination x -> text x
    PeerAddress           x -> text x
    PeerHostname          x -> text x
    PeerIPv4              x -> string . show $ x
    PeerIPv6              x -> string . show $ x
    PeerPort              x -> toEncoding x
    PeerService           x -> text x
    SamplingPriority      x -> word8 x
    SpanKind              x -> text (spanKindLabel x)
    SomeTag             _ x -> text x

micros :: POSIXTime -> Word64
micros = round . (1000000 *)
