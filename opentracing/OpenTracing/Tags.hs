{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE ViewPatterns               #-}

module OpenTracing.Tags
    ( Tags(fromTags)
    , Tag
    , TagVal(..)
    , setTag
    , getTag
    , getTagReify

    -- * Standard span tags.
    -- | Refer to the [OpenTracing spec](https://github.com/opentracing/specification/blob/master/semantic_conventions.md#span-tags-table)
    -- for more info.
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

    , _Component
    , _DbInstance
    , _DbStatement
    , _DbType
    , _DbUser
    , _Error
    , _HttpMethod
    , _HttpStatusCode
    , _HttpUrl
    , _MessageBusDestination
    , _PeerAddress
    , _PeerHostname
    , _PeerIPv4
    , _PeerIPv6
    , _PeerPort
    , _PeerService
    , _SamplingPriority
    , _SpanKind

    , SpanKinds(..)
    , spanKindLabel
    )
where

import           Control.Lens
import           Data.Aeson                  (ToJSON (..))
import           Data.Aeson.Encoding
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.ByteString.Lazy        as Lazy
import           Data.HashMap.Strict         (HashMap)
import qualified Data.HashMap.Strict         as HashMap
import           Data.Int                    (Int64)
import           Data.Monoid                 (First)
import           Data.String                 (IsString)
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Data.Text.Encoding          (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy.Encoding     as Lazy
import           Data.Word                   (Word8)
import           Network.HTTP.Types
import           OpenTracing.Types
import           Text.Read                   (readMaybe)

pattern ComponentKey :: forall a. (Eq a, IsString a) => a
pattern DbInstanceKey :: forall a. (Eq a, IsString a) => a
pattern DbStatementKey :: forall a. (Eq a, IsString a) => a
pattern DbTypeKey :: forall a. (Eq a, IsString a) => a
pattern DbUserKey :: forall a. (Eq a, IsString a) => a
pattern ErrorKey :: forall a. (Eq a, IsString a) => a
pattern HttpMethodKey :: forall a. (Eq a, IsString a) => a
pattern HttpStatusCodeKey :: forall a. (Eq a, IsString a) => a
pattern HttpUrlKey :: forall a. (Eq a, IsString a) => a
pattern MessageBusDestinationKey :: forall a. (Eq a, IsString a) => a
pattern PeerAddressKey :: forall a. (Eq a, IsString a) => a
pattern PeerHostnameKey :: forall a. (Eq a, IsString a) => a
pattern PeerIPv4Key :: forall a. (Eq a, IsString a) => a
pattern PeerIPv6Key :: forall a. (Eq a, IsString a) => a
pattern PeerPortKey :: forall a. (Eq a, IsString a) => a
pattern PeerServiceKey :: forall a. (Eq a, IsString a) => a
pattern SamplingPriorityKey :: forall a. (Eq a, IsString a) => a
pattern SpanKindKey :: forall a. (Eq a, IsString a) => a
pattern Component :: Text -> Tag
pattern DbInstance :: Text -> Tag
pattern DbStatement :: Text -> Tag
pattern DbType :: Text -> Tag
pattern DbUser :: Text -> Tag
pattern Error :: Bool -> Tag
pattern HttpUrl :: Text -> Tag
pattern MessageBusDestination :: Text -> Tag
pattern PeerAddress :: Text -> Tag
pattern PeerHostname :: Text -> Tag
pattern PeerService :: Text -> Tag
pattern HttpMethod :: Method -> Tag
pattern HttpStatusCode :: Status -> Tag
pattern PeerIPv4 :: IPv4 -> Tag
pattern PeerIPv6 :: IPv6 -> Tag
pattern PeerPort :: Port -> Tag
pattern SamplingPriority :: Word8 -> Tag
pattern SpanKind :: SpanKinds -> Tag

-- | Tags are structured data associated with a `OpenTracing.Span.Span`. They can give
-- a more complete picture of what a Span is doing than the operation alone. Tags
-- apply to the entire timerange of a Span. Use `OpenTracing.Log.LogField` for
-- events that refer to particular timestamp.
newtype Tags = Tags { fromTags :: HashMap Text TagVal }
    deriving (Eq, Show, Semigroup, Monoid, ToJSON)

-- | A Tag is a key:value pair
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

-- | Get a tag and attempt to convert it from a serialized format
getTagReify :: Getting (First b) Tag b -> Text -> Tags -> Maybe b
getTagReify p k ts = getTag k ts >>= preview p . (k,)


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
spanKindLabel RPCClient = "CLIENT"
spanKindLabel RPCServer = "SERVER"
spanKindLabel Producer  = "PRODUCER"
spanKindLabel Consumer  = "CONSUMER"

fromSpanKindLabel :: Text -> Maybe SpanKinds
fromSpanKindLabel "CLIENT"   = Just RPCClient
fromSpanKindLabel "SERVER"   = Just RPCServer
fromSpanKindLabel "PRODUCER" = Just Producer
fromSpanKindLabel "CONSUMER" = Just Consumer
fromSpanKindLabel _          = Nothing

instance ToJSON SpanKinds where
    toJSON     = toJSON . spanKindLabel
    toEncoding = text   . spanKindLabel
