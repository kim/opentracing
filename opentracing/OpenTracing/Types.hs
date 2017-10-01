{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}

module OpenTracing.Types
    ( TextMap(..)
    , textMap
    , HttpHeaders(..)
    , httpHeaders

    , AsCarrier(..)

    , traceInject
    , traceExtract

    , Span
    , newSpan

    , ActiveSpan
    , mkActive
    , modifyActiveSpan
    , readActiveSpan

    , FinishedSpan
    , defaultTraceFinish

    , spanContext
    , spanOperation
    , spanStart
    , spanTags
    , spanRefs
    , spanLogs
    , spanDuration

    , Sampled(..)
    , HasSampled(..)

    , SpanOpts(..)
    , spanOpts

    , Reference(..)
    , Tag(..)
    , tagLabel
    , SpanKinds(..)
    , spanKindLabel

    , LogRecord(..)
    , logTime
    , logFields

    , LogField(..)
    , logFieldLabel

    , IPv4(..)
    , IPv6(..)
    , Port(..)
    )
where

import           Control.Exception      (Exception)
import           Control.Lens           hiding (op)
import           Control.Monad.IO.Class
import           Data.Aeson             (ToJSON (..), object)
import           Data.Aeson.Encoding
import           Data.Hashable
import           Data.HashMap.Strict    (HashMap)
import           Data.HashSet           (HashSet)
import qualified Data.HashSet           as HashSet
import           Data.IORef
import qualified Data.IP                as IP
import           Data.List.NonEmpty     (NonEmpty)
import           Data.Monoid
import           Data.Set               (Set)
import qualified Data.Set               as Set
import           Data.Text              (Text)
import           Data.Text.Encoding     (decodeUtf8)
import           Data.Time.Clock
import           Data.Word
import           GHC.Generics           (Generic)
import           GHC.Stack
import           Network.HTTP.Types     (Header, Method, Status, statusCode)
import           Prelude                hiding (span)


newtype TextMap ctx = TextMap { fromTextMap :: HashMap Text Text }
    deriving (Eq, Show, Monoid)

newtype HttpHeaders ctx = HttpHeaders { fromHttpHeaders :: [Header] }
    deriving (Eq, Show, Monoid)

{-
   skip for now: not clear what this is for (the whole stack would need to
   agree on the encoding)
newtype Binary ctx = Binary { fromBinary :: Lazy.ByteString }
    deriving (Eq, Show, Monoid)

binary :: Binary ctx
binary = mempty
-}

textMap :: TextMap ctx
textMap = mempty

httpHeaders :: HttpHeaders ctx
httpHeaders = mempty


class AsCarrier a ctx | a -> ctx where
    _Carrier :: Prism' (a ctx) ctx

traceInject :: AsCarrier a ctx => ctx -> a ctx
traceInject = review _Carrier

traceExtract :: AsCarrier a ctx => a ctx -> Maybe ctx
traceExtract = preview _Carrier


data Span ctx = Span
    { _spanContext   :: ctx
    , _spanOperation :: Text
    , _spanStart     :: UTCTime
    , _spanTags      :: Set Tag
    , _spanRefs      :: HashSet (Reference ctx)
    , _spanLogs      :: [LogRecord]
    } deriving Show

newSpan
    :: ( MonadIO  m
       , Eq       ctx
       , Hashable ctx
       )
    => ctx
    -> Text
    -> [Reference ctx]
    -> [Tag]
    -> m (Span ctx)
newSpan ctx op rs ts = do
    t <- liftIO getCurrentTime
    pure Span
        { _spanContext   = ctx
        , _spanOperation = op
        , _spanStart     = t
        , _spanTags      = Set.fromList ts
        , _spanRefs      = HashSet.fromList rs
        , _spanLogs      = mempty
        }


data ActiveSpan ctx = ActiveSpan
    { _activeSpan    :: Span ctx
    , mutActiveSpan :: IORef (Span ctx)
    }

mkActive :: Span ctx -> IO (ActiveSpan ctx)
mkActive s = ActiveSpan s <$> newIORef s

modifyActiveSpan :: ActiveSpan ctx -> (Span ctx -> Span ctx) -> IO ()
modifyActiveSpan ActiveSpan{mutActiveSpan} f
    = atomicModifyIORef' mutActiveSpan ((,()) . f)

readActiveSpan :: ActiveSpan ctx -> IO (Span ctx)
readActiveSpan = readIORef . mutActiveSpan


data FinishedSpan ctx = FinishedSpan
    { _spanSpan     :: Span ctx
    , _spanDuration :: NominalDiffTime
    } deriving Show

defaultTraceFinish :: MonadIO m => Span ctx -> m (FinishedSpan ctx)
defaultTraceFinish s = do
    t <- liftIO getCurrentTime
    pure FinishedSpan
        { _spanSpan     = s
        , _spanDuration = diffUTCTime t (_spanStart s)
        }


data Sampled = NotSampled | Sampled
    deriving (Eq, Show, Read, Bounded, Enum, Generic)

instance Hashable Sampled

instance ToJSON Sampled where
    toJSON     = toJSON . fromEnum
    toEncoding = int . fromEnum

class HasSampled ctx where
    ctxSampled :: Lens' ctx Sampled


data SpanOpts ctx = SpanOpts
    { spanOptOperation :: Text
    , spanOptRefs      :: [Reference ctx]
    , spanOptTags      :: [Tag]
    , spanOptSampled   :: Maybe Sampled
    -- ^ Force 'Span' to be sampled (or not).
    -- 'Nothing' denotes leave decision to 'Sampler'
    }

spanOpts :: Text -> [Reference ctx] -> SpanOpts ctx
spanOpts op ref = SpanOpts
    { spanOptOperation = op
    , spanOptRefs      = ref
    , spanOptTags      = mempty
    , spanOptSampled   = Nothing
    }


data Reference ctx
    = ChildOf     { refCtx :: ctx }
    | FollowsFrom { refCtx :: ctx }
    deriving (Eq, Show, Generic)

instance Hashable ctx => Hashable (Reference ctx)


data Tag
    = Component             Text
    | DbInstance            Text
    | DbStatement           Text
    | DbType                Text
    | DbUser                Text
    | Error                 Bool
    | HttpMethod            Method
    | HttpStatusCode        Status
    | HttpUrl               Text
    | MessageBusDestination Text
    | PeerAddress           Text
    | PeerHostname          Text
    | PeerIPv4              IPv4
    | PeerIPv6              IPv6
    | PeerPort              Port
    | PeerService           Text
    | SamplingPriority      Word8
    | SpanKind              SpanKinds

    | SomeTag Text          Text
    deriving (Eq, Show, Ord)

tagLabel :: Tag -> Text
tagLabel (Component             _) = "component"
tagLabel (DbInstance            _) = "db.instance"
tagLabel (DbStatement           _) = "db.statement"
tagLabel (DbType                _) = "db.type"
tagLabel (DbUser                _) = "db.user"
tagLabel (Error                 _) = "error"
tagLabel (HttpMethod            _) = "http.method"
tagLabel (HttpStatusCode        _) = "http.status_code"
tagLabel (HttpUrl               _) = "http.url"
tagLabel (MessageBusDestination _) = "message_bus.destination"
tagLabel (PeerAddress           _) = "peer.address"
tagLabel (PeerHostname          _) = "peer.hostname"
tagLabel (PeerIPv4              _) = "peer.ipv4"
tagLabel (PeerIPv6              _) = "peer.ipv6"
tagLabel (PeerPort              _) = "peer.port"
tagLabel (PeerService           _) = "peer.service"
tagLabel (SamplingPriority      _) = "sampling.priority"
tagLabel (SpanKind              _) = "span.kind"
tagLabel (SomeTag             x _) = x

instance ToJSON Tag where
    toJSON     t = object . (:[]) . (tagLabel t,) $ case t of
        Component             x -> toJSON x
        DbInstance            x -> toJSON x
        DbStatement           x -> toJSON x
        DbType                x -> toJSON x
        DbUser                x -> toJSON x
        Error                 x -> toJSON x
        HttpMethod            x -> toJSON . decodeUtf8 $ x
        HttpStatusCode        x -> toJSON . statusCode $ x
        HttpUrl               x -> toJSON x
        MessageBusDestination x -> toJSON x
        PeerAddress           x -> toJSON x
        PeerHostname          x -> toJSON x
        PeerIPv4              x -> toJSON . show $ x
        PeerIPv6              x -> toJSON . show $ x
        PeerPort              x -> toJSON x
        PeerService           x -> toJSON x
        SamplingPriority      x -> toJSON x
        SpanKind              x -> toJSON x
        SomeTag             _ x -> toJSON x

    toEncoding t = pairs . pair (tagLabel t) $ case t of
        Component             x -> text x
        DbInstance            x -> text x
        DbStatement           x -> text x
        DbType                x -> text x
        DbUser                x -> text x
        Error                 x -> bool x
        HttpMethod            x -> text . decodeUtf8 $ x
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
        SpanKind              x -> toEncoding x
        SomeTag             _ x -> text x

data SpanKinds
    = RPCClient
    | RPCServer
    | Producer
    | Consumer
    deriving (Eq, Show, Ord)

spanKindLabel :: SpanKinds -> Text
spanKindLabel RPCClient = "client"
spanKindLabel RPCServer = "server"
spanKindLabel Producer  = "producer"
spanKindLabel Consumer  = "consumer"

instance ToJSON SpanKinds where
    toJSON     = toJSON . spanKindLabel
    toEncoding = text   . spanKindLabel


data LogRecord = LogRecord
    { _logTime   :: UTCTime
    , _logFields :: NonEmpty LogField
    } deriving Show

data LogField where
    LogField :: Show      a => Text      -> a -> LogField
    Event    ::                Text           -> LogField
    Message  ::                Text           -> LogField
    Stack    ::                CallStack      -> LogField
    ErrKind  ::                Text           -> LogField
    ErrObj   :: Exception e => e              -> LogField

deriving instance (Show LogField)

logFieldLabel :: LogField -> Text
logFieldLabel (LogField x _) = x
logFieldLabel (Event      _) = "event"
logFieldLabel (Message    _) = "message"
logFieldLabel (Stack      _) = "stack"
logFieldLabel (ErrKind    _) = "error.kind"
logFieldLabel (ErrObj     _) = "error.object"


newtype IPv4 = IPv4 { fromIPv4 :: IP.IPv4 }
    deriving (Bounded, Enum, Eq, Ord)

newtype IPv6 = IPv6 { fromIPv6 :: IP.IPv6 }
    deriving (Bounded, Enum, Eq, Ord)

instance Show IPv4 where show = show . fromIPv4
instance Show IPv6 where show = show . fromIPv6

instance Read IPv4 where readsPrec p = map (over _1 IPv4) . readsPrec p
instance Read IPv6 where readsPrec p = map (over _1 IPv6) . readsPrec p

instance ToJSON IPv4 where
    toJSON     = toJSON . show . fromIPv4
    toEncoding = string . show . fromIPv4

instance ToJSON IPv6 where
    toJSON     = toJSON . show . fromIPv6
    toEncoding = string . show . fromIPv6


newtype Port = Port { fromPort :: Word16 }
    deriving (Enum, Eq, Num, Ord)

instance Show Port where show = show . fromPort
instance Read Port where readsPrec p = map (over _1 Port) . readsPrec p

instance ToJSON Port where
    toJSON     = toJSON . fromPort
    toEncoding = word16 . fromPort

makeClassy ''Span
makeLenses ''ActiveSpan
makeLenses ''FinishedSpan
makeLenses ''LogRecord

instance HasSpan (FinishedSpan ctx) ctx where
    span = spanSpan

instance HasSpan (ActiveSpan ctx) ctx where
    span = activeSpan

instance HasSampled ctx => HasSampled (Span ctx) where
    ctxSampled = spanContext . ctxSampled

instance HasSampled ctx => HasSampled (FinishedSpan ctx) where
    ctxSampled = spanContext . ctxSampled
