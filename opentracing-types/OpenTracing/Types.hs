{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}

module OpenTracing.Types
    ( ConfigSource(..)

    , TextMap(..)
    , textMap
    , HttpHeaders(..)
    , httpHeaders
    , Binary(..)
    , binary

    , AsCarrier(..)

    , traceInject
    , traceExtract

    , Span
    , newSpan
    , FinishedSpan
    , traceFinish
    , spanContext
    , spanOperation
    , spanStart
    , spanTags
    , spanRefs
    , spanLogs
    , spanDuration

    , Reference(..)
    , Tag(..)
    , tagLabel

    , LogRecord
    , logTime
    , logFields

    , LogField(..)
    , logFieldLabel
    )
where

import           Control.Lens           hiding (op)
import           Control.Monad.IO.Class
import qualified Data.ByteString.Lazy   as Lazy
import           Data.Hashable
import           Data.HashMap.Strict    (HashMap)
import           Data.HashSet           (HashSet)
import           Data.IP                (IPv4, IPv6)
import           Data.List.NonEmpty     (NonEmpty)
import           Data.Monoid
import           Data.Set               (Set)
import           Data.Text              (Text)
import           Data.Time.Clock
import           Data.Word
import           GHC.Generics           (Generic)
import           GHC.Stack
import           Network                (PortNumber)
import           Network.HTTP.Types     (Header, Status, StdMethod)
import           Prelude                hiding (span)


data ConfigSource = FromEnv | FromFile FilePath
    deriving (Eq, Show)


newtype TextMap ctx = TextMap { fromTextMap :: HashMap Text Text }
    deriving (Eq, Show, Monoid)

newtype HttpHeaders ctx = HttpHeaders { fromHttpHeaders :: [Header] }
    deriving (Eq, Show, Monoid)

newtype Binary ctx = Binary { fromBinary :: Lazy.ByteString }
    deriving (Eq, Show, Monoid)

textMap :: TextMap ctx
textMap = mempty

httpHeaders :: HttpHeaders ctx
httpHeaders = mempty

binary :: Binary ctx
binary = mempty

class AsCarrier a ctx | a -> ctx where
    _Carrier :: Prism' a ctx

traceInject :: AsCarrier a ctx => ctx -> a
traceInject = review _Carrier

traceExtract :: AsCarrier a ctx => a -> Maybe ctx
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
    :: MonadIO m
    => ctx
    -> Text
    -> HashSet (Reference ctx)
    -> Set Tag
    -> m (Span ctx)
newSpan ctx op rs ts = do
    t <- liftIO getCurrentTime
    pure Span
        { _spanContext   = ctx
        , _spanOperation = op
        , _spanStart     = t
        , _spanTags      = ts
        , _spanRefs      = rs
        , _spanLogs      = mempty
        }


data FinishedSpan ctx = FinishedSpan
    { _spanSpan     :: Span ctx
    , _spanDuration :: NominalDiffTime
    } deriving Show

traceFinish :: MonadIO m => Span ctx -> m (FinishedSpan ctx)
traceFinish s = do
    t <- liftIO getCurrentTime
    pure FinishedSpan
        { _spanSpan     = s
        , _spanDuration = diffUTCTime t (_spanStart s)
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
    | HttpMethod            StdMethod
    | HttpStatusCode        Status
    | HttpUrl               Text
    | MessageBusDestination Text
    | PeerAddress           Text
    | PeerHostname          Text
    | PeerIPv4              IPv4
    | PeerIPv6              IPv6
    | PeerPort              PortNumber
    | PeerService           Text
    | SamplingPriority      Word8
    | SpanKind              Text

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

data LogRecord = LogRecord
    { _logTime   :: UTCTime
    , _logFields :: NonEmpty LogField
    } deriving Show

data LogField where
    LogField :: Show a => Text      -> a -> LogField
    Event    ::           Text           -> LogField
    Message  ::           Text           -> LogField
    Stack    ::           CallStack      -> LogField
    ErrKind  ::           Text           -> LogField

deriving instance (Show LogField)

logFieldLabel :: LogField -> Text
logFieldLabel (LogField x _) = x
logFieldLabel (Event      _) = "event"
logFieldLabel (Message    _) = "message"
logFieldLabel (Stack      _) = "stack"
logFieldLabel (ErrKind    _) = "error.kind"


makeClassy ''Span
makeLenses ''FinishedSpan
makeLenses ''LogRecord

instance HasSpan (FinishedSpan ctx) ctx where
    span = spanSpan
