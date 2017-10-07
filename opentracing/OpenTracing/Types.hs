{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}

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

    , Tags
    , Tag
    , TagVal(..)
    , setTag
    , getTag

    , pattern ComponentKey
    , pattern DbInstanceKey
    , pattern DbStatementKey
    , pattern DbTypeKey
    , pattern DbUserKey
    , pattern ErrorKey
    , pattern HttpMethodKey
    , pattern HttpStatusCodeKey
    , pattern HttpUrlKey
    , pattern MessageBusDestinationKey
    , pattern PeerAddressKey
    , pattern PeerHostnameKey
    , pattern PeerIPv4Key
    , pattern PeerIPv6Key
    , pattern PeerPortKey
    , pattern PeerServiceKey
    , pattern SamplingPriorityKey
    , pattern SpanKindKey

    , pattern Component
    , pattern DbInstance
    , pattern DbStatement
    , pattern DbType
    , pattern DbUser
    , pattern Error
    , pattern HttpMethod
    , pattern HttpStatusCode
    , pattern HttpUrl
    , pattern MessageBusDestination
    , pattern PeerAddress
    , pattern PeerHostname
    , pattern PeerIPv4
    , pattern PeerIPv6
    , pattern PeerPort
    , pattern PeerService
    , pattern SamplingPriority
    , pattern SpanKind

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

import           Control.Exception           (Exception)
import           Control.Lens                hiding (op)
import           Control.Monad.IO.Class
import           Data.Aeson                  (ToJSON (..))
import           Data.Aeson.Encoding
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.ByteString.Lazy        as Lazy
import           Data.Foldable
import           Data.Hashable
import           Data.HashMap.Strict         (HashMap)
import qualified Data.HashMap.Strict         as HashMap
import           Data.HashSet                (HashSet)
import qualified Data.HashSet                as HashSet
import           Data.Int                    (Int64)
import           Data.IORef
import qualified Data.IP                     as IP
import           Data.List.NonEmpty          (NonEmpty)
import           Data.Monoid
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Data.Text.Encoding          (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy.Encoding     as Lazy
import           Data.Time.Clock
import           Data.Word
import           GHC.Generics                (Generic)
import           GHC.Stack
import           Network.HTTP.Types
import           Prelude                     hiding (span)
import           Text.Read                   (readMaybe)


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


class AsCarrier a x ctx | a x -> ctx where
    _Carrier :: Prism' (a x) ctx

traceInject :: AsCarrier a ctx ctx => ctx -> a ctx
traceInject = review _Carrier

traceExtract :: AsCarrier a ctx ctx => a ctx -> Maybe ctx
traceExtract = preview _Carrier


data Span ctx = Span
    { _spanContext   :: ctx
    , _spanOperation :: Text
    , _spanStart     :: UTCTime
    , _spanTags      :: Tags
    , _spanRefs      :: HashSet (Reference ctx)
    , _spanLogs      :: [LogRecord]
    } deriving Show

newSpan
    :: ( MonadIO     m
       , Eq          ctx
       , Hashable    ctx
       , Foldable    t
       , Foldable    u
       )
    => ctx
    -> Text
    -> t (Reference ctx)
    -> u Tag
    -> m (Span ctx)
newSpan ctx op rs ts = do
    t <- liftIO getCurrentTime
    pure Span
        { _spanContext   = ctx
        , _spanOperation = op
        , _spanStart     = t
        , _spanTags      = foldMap (`setTag` mempty) ts
        , _spanRefs      = HashSet.fromList . toList $ rs
        , _spanLogs      = mempty
        }


data ActiveSpan ctx = ActiveSpan
    { _activeSpan   :: Span ctx
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


newtype Tags = Tags { fromTags :: HashMap Text TagVal }
    deriving (Eq, Show, Monoid, ToJSON)

type Tag = (Text, TagVal)

data TagVal
    = BoolT   Bool
    | StringT Text
    | IntT    Int64
    | DoubleT Double
    | BinaryT Lazy.ByteString
    deriving (Eq, Show)

instance ToJSON TagVal where
    toJSON (BoolT   x) = toJSON x
    toJSON (StringT x) = toJSON x
    toJSON (IntT    x) = toJSON x
    toJSON (DoubleT x) = toJSON x
    toJSON (BinaryT x) = toJSON . Lazy.decodeUtf8 . B64.encode $ x

    toEncoding (BoolT   x) = toEncoding x
    toEncoding (StringT x) = toEncoding x
    toEncoding (IntT    x) = toEncoding x
    toEncoding (DoubleT x) = toEncoding x
    toEncoding (BinaryT x) = toEncoding . Lazy.decodeUtf8 . B64.encode $ x

setTag :: Tag -> Tags -> Tags
setTag (k,v) = Tags . HashMap.insert k v . fromTags

getTag :: Text -> Tags -> Maybe TagVal
getTag k = HashMap.lookup k . fromTags

pattern ComponentKey             = "component"
pattern DbInstanceKey            = "db.instance"
pattern DbStatementKey           = "db.statement"
pattern DbTypeKey                = "db.type"
pattern DbUserKey                = "db.user"
pattern ErrorKey                 = "error"
pattern HttpMethodKey            = "http.method"
pattern HttpStatusCodeKey        = "http.status_code"
pattern HttpUrlKey               = "http.url"
pattern MessageBusDestinationKey = "message_bus.destination"
pattern PeerAddressKey           = "peer.address"
pattern PeerHostnameKey          = "peer.hostname"
pattern PeerIPv4Key              = "peer.ipv4"
pattern PeerIPv6Key              = "peer.ipv6"
pattern PeerPortKey              = "peer.port"
pattern PeerServiceKey           = "peer.service"
pattern SamplingPriorityKey      = "sampling.priority"
pattern SpanKindKey              = "span.kind"

_Component :: Prism' Tag Text
_Component = prism' ((ComponentKey,) . StringT) $ \case
    (k, StringT v) | k == ComponentKey -> Just v
    _ -> Nothing

pattern Component v <- (preview _Component -> Just v) where
    Component v = review _Component v

_DbInstance :: Prism' Tag Text
_DbInstance = prism' ((DbInstanceKey,) . StringT) $ \case
    (k, StringT v) | k == DbInstanceKey -> Just v
    _ -> Nothing

pattern DbInstance v <- (preview _DbInstance -> Just v) where
    DbInstance v = review _DbInstance v

_DbStatement :: Prism' Tag Text
_DbStatement = prism' ((DbStatementKey,) . StringT) $ \case
    (k, StringT v) | k == DbStatementKey -> Just v
    _ -> Nothing

pattern DbStatement v <- (preview _DbStatement -> Just v) where
    DbStatement v = review _DbStatement v

_DbType :: Prism' Tag Text
_DbType = prism' ((DbTypeKey,) . StringT) $ \case
    (k, StringT v) | k == DbTypeKey -> Just v
    _ -> Nothing

pattern DbType v <- (preview _DbType -> Just v) where
    DbType v = review _DbType v

_DbUser :: Prism' Tag Text
_DbUser = prism' ((DbUserKey,) . StringT) $ \case
    (k, StringT v) | k == DbUserKey -> Just v
    _ -> Nothing

pattern DbUser v <- (preview _DbUser -> Just v) where
    DbUser v = review _DbUser v

_Error :: Prism' Tag Bool
_Error = prism' ((ErrorKey,) . BoolT) $ \case
    (k, BoolT v) | k == ErrorKey -> Just v
    _ -> Nothing

pattern Error v <- (preview _Error -> Just v) where
    Error v = review _Error v

_HttpUrl :: Prism' Tag Text
_HttpUrl = prism' ((HttpUrlKey,) . StringT) $ \case
    (k, StringT v) | k == HttpUrlKey -> Just v
    _ -> Nothing

pattern HttpUrl v <- (preview _HttpUrl -> Just v) where
    HttpUrl v = review _HttpUrl v

_MessageBusDestination :: Prism' Tag Text
_MessageBusDestination = prism' ((MessageBusDestinationKey,) . StringT) $ \case
    (k, StringT v) | k == MessageBusDestinationKey -> Just v
    _ -> Nothing

pattern MessageBusDestination v <- (preview _MessageBusDestination -> Just v) where
    MessageBusDestination v = review _MessageBusDestination v

_PeerAddress :: Prism' Tag Text
_PeerAddress = prism' ((PeerAddressKey,) . StringT) $ \case
    (k, StringT v) | k == PeerAddressKey -> Just v
    _ -> Nothing

pattern PeerAddress v <- (preview _PeerAddress -> Just v) where
    PeerAddress v = review _PeerAddress v

_PeerHostname :: Prism' Tag Text
_PeerHostname = prism' ((PeerHostnameKey,) . StringT) $ \case
    (k, StringT v) | k == PeerHostnameKey -> Just v
    _ -> Nothing

pattern PeerHostname v <- (preview _PeerHostname -> Just v) where
    PeerHostname v = review _PeerHostname v

_PeerService :: Prism' Tag Text
_PeerService = prism' ((PeerServiceKey,) . StringT) $ \case
    (k, StringT v) | k == PeerServiceKey -> Just v
    _ -> Nothing

pattern PeerService v <- (preview _PeerService -> Just v) where
    PeerService v = review _PeerService v

_HttpMethod :: Prism' Tag Method
_HttpMethod = prism' ((HttpMethodKey,) . StringT . decodeUtf8) $ \case
    (k, StringT (encodeUtf8 -> x)) | k == HttpMethodKey ->
        either (const Nothing) (const (Just x)) $ parseMethod x
    _ -> Nothing

pattern HttpMethod v <- (preview _HttpMethod -> Just v) where
    HttpMethod v = review _HttpMethod v

_HttpStatusCode :: Prism' Tag Status
_HttpStatusCode = prism' ((HttpStatusCodeKey,) . IntT . fromIntegral . statusCode) $ \case
    (k, IntT x) | k == HttpStatusCodeKey -> Just . toEnum . fromIntegral $ x
    _ -> Nothing

pattern HttpStatusCode v <- (preview _HttpStatusCode -> Just v) where
    HttpStatusCode v = review _HttpStatusCode v

_PeerIPv4 :: Prism' Tag IPv4
_PeerIPv4 = prism' ((PeerIPv4Key,) . StringT . Text.pack . show) $ \case
    (k, StringT x) | k == PeerIPv4Key -> readMaybe (Text.unpack x)
    _ -> Nothing

pattern PeerIPv4 v <- (preview _PeerIPv4 -> Just v) where
    PeerIPv4 v = review _PeerIPv4 v

_PeerIPv6 :: Prism' Tag IPv6
_PeerIPv6 = prism' ((PeerIPv6Key,) . StringT . Text.pack . show) $ \case
    (k, StringT x) | k == PeerIPv6Key -> readMaybe (Text.unpack x)
    _ -> Nothing

pattern PeerIPv6 v <- (preview _PeerIPv6 -> Just v) where
    PeerIPv6 v = review _PeerIPv6 v

_PeerPort :: Prism' Tag Port
_PeerPort = prism' ((PeerPortKey,) . IntT . fromIntegral . fromPort) $ \case
    (k, IntT x) | k == PeerPortKey -> Just . toEnum . fromIntegral $ x
    _ -> Nothing

pattern PeerPort v <- (preview _PeerPort -> Just v) where
    PeerPort v = review _PeerPort v

_SamplingPriority :: Prism' Tag Word8
_SamplingPriority = prism' ((SamplingPriorityKey,) . IntT . fromIntegral) $ \case
    (k, IntT x) | k == SamplingPriorityKey -> Just . fromIntegral $ x
    _ -> Nothing

pattern SamplingPriority v <- (preview _SamplingPriority -> Just v) where
    SamplingPriority v = review _SamplingPriority v

_SpanKind :: Prism' Tag SpanKinds
_SpanKind = prism' ((SpanKindKey,) . StringT . spanKindLabel) $ \case
    (k, StringT x) | k == SpanKindKey -> fromSpanKindLabel x
    _ -> Nothing

pattern SpanKind v <- (preview _SpanKind -> Just v) where
    SpanKind = review _SpanKind

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

fromSpanKindLabel :: Text -> Maybe SpanKinds
fromSpanKindLabel "client"   = Just RPCClient
fromSpanKindLabel "server"   = Just RPCServer
fromSpanKindLabel "producer" = Just Producer
fromSpanKindLabel "consumer" = Just Consumer
fromSpanKindLabel _          = Nothing

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
