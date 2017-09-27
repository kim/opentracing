{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}

module OpenTracing.Simple
    ( Sampled(..)

    , Context
    , ctxTraceID
    , ctxSpanID
    , ctxSampled
    , ctxBaggage

    , Env
    , newEnv

    , simpleTracer
    , simpleReporter
    )
where

import           Codec.Serialise
import           Control.Lens               hiding (Context, (.=))
import           Control.Monad.Reader
import           Data.Aeson                 hiding (Error)
import           Data.Aeson.Encoding
import           Data.ByteString.Lazy.Char8 (putStrLn)
import qualified Data.CaseInsensitive       as CI
import           Data.Foldable              (toList)
import           Data.Hashable
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HashMap
import           Data.Monoid
import           Data.Text                  (Text, isPrefixOf, toLower)
import           Data.Text.Encoding         (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy.Builder     as TB
import qualified Data.Text.Lazy.Builder.Int as TB
import qualified Data.Text.Read             as Text
import           Data.Word
import           GHC.Generics               (Generic)
import           GHC.Stack                  (prettyCallStack)
import           Network.HTTP.Types         (statusCode)
import           OpenTracing.Class
import           OpenTracing.Types
import           Prelude                    hiding (putStrLn)
import           System.Random.MWC


data Sampled = Sampled | NotSampled
    deriving (Eq, Show, Read, Generic)

instance Hashable  Sampled
instance Serialise Sampled

instance ToJSON Sampled where
    toEncoding Sampled    = word8 1
    toEncoding NotSampled = word8 0

    toJSON Sampled    = toJSON (1 :: Word8)
    toJSON NotSampled = toJSON (0 :: Word8)

newtype PRNG = PRNG { unPRNG :: GenIO }

type TraceID = Word64
type SpanID  = Word64

type Context = SimpleContext

data SimpleContext = SimpleContext
    { ctxTraceID  :: TraceID
    , ctxSpanID   :: SpanID
    , _ctxSampled :: Sampled
    , _ctxBaggage :: HashMap Text Text
    } deriving (Eq, Show, Generic)

instance Hashable  SimpleContext
instance Serialise SimpleContext

instance ToJSON SimpleContext where
    toEncoding c = pairs $
           "trace_id" .= ctxTraceID  c
        <> "span_id"  .= ctxSpanID   c
        <> "sampled"  .= _ctxSampled c
        <> "baggage"  .= _ctxBaggage c

    toJSON c = object
        [ "trace_id" .= ctxTraceID  c
        , "span_id"  .= ctxSpanID   c
        , "sampled"  .= _ctxSampled c
        , "baggage"  .= _ctxBaggage c
        ]


instance AsCarrier (TextMap SimpleContext) SimpleContext where
    _Carrier = prism' fromCtx toCtx
      where
        fromCtx SimpleContext{..} = TextMap . HashMap.fromList $
              ("ot-tracer-traceid", review _ID ctxTraceID)
            : ("ot-tracer-spanid" , review _ID ctxSpanID)
            : ("ot-tracer-sampled", review _Sampled _ctxSampled)
            : map (over _1 ("ot-baggage-" <>)) (HashMap.toList _ctxBaggage)

        toCtx (TextMap m) = SimpleContext
            <$> (HashMap.lookup "ot-tracer-traceid" m >>= preview _ID)
            <*> (HashMap.lookup "ot-tracer-spanid"  m >>= preview _ID)
            <*> (HashMap.lookup "ot-tracer-sampled" m >>= preview _Sampled)
            <*> pure (HashMap.filterWithKey (\k _ -> "ot-baggage-" `isPrefixOf` k) m)


instance AsCarrier (HttpHeaders SimpleContext) SimpleContext where
    _Carrier = prism' fromCtx toCtx
      where
        fromCtx
            = HttpHeaders
            . map (bimap (CI.mk . encodeUtf8) encodeUtf8)
            . HashMap.toList
            . fromTextMap
            . (review _Carrier :: Context -> TextMap Context)

        toCtx
            = (preview _Carrier :: TextMap Context -> Maybe Context)
            . TextMap
            . HashMap.fromList
            . map (bimap (toLower . decodeUtf8 . CI.original) decodeUtf8)
            . fromHttpHeaders

instance AsCarrier (Binary SimpleContext) SimpleContext where
    _Carrier = prism' fromCtx toCtx
      where
        fromCtx = Binary . serialise
        toCtx   = either (const Nothing) pure . deserialiseOrFail . fromBinary


data Env = Env
    { envPRNG :: PRNG
    }

newEnv :: MonadIO m => m Env
newEnv = Env <$> newPRNG

instance MonadIO m => MonadTrace SimpleContext (ReaderT Env m) where
    traceStart = start

instance MonadIO m => MonadReport SimpleContext m where
    traceReport = report

simpleTracer :: Env -> Interpret (MonadTrace SimpleContext) MonadIO
simpleTracer r = Interpret $ \m -> runReaderT m r

simpleReporter :: Interpret (MonadReport SimpleContext) MonadIO
simpleReporter = Interpret id

--------------------------------------------------------------------------------
-- Internal

start :: (MonadIO m, MonadReader Env m) => SpanOpts Context -> m (Span Context)
start SpanOpts{..} = do
    ctx <- case spanOptRefs of
               []    -> freshContext
               (p:_) -> fromParent p
    newSpan ctx spanOptOperation spanOptRefs spanOptTags

report :: MonadIO m => FinishedSpan Context -> m ()
report = liftIO . putStrLn . encodingToLazyByteString . spanE


newPRNG :: MonadIO m => m PRNG
newPRNG = PRNG <$> liftIO createSystemRandom

newTraceID :: MonadIO m => PRNG -> m TraceID
newTraceID = liftIO . uniform . unPRNG

newSpanID :: MonadIO m => PRNG -> m SpanID
newSpanID = liftIO . uniform . unPRNG

_ID :: Prism' Text Word64
_ID = prism' enc dec
  where
    enc = view strict . TB.toLazyText . TB.decimal
    dec = either (const Nothing) (pure . fst) . Text.decimal
{-# INLINE _ID #-}

_Sampled :: Prism' Text Sampled
_Sampled = prism' enc dec
    where
      enc = \case Sampled -> "1"
                  _       -> "0"

      dec = either (const Nothing) id
          . fmap (\(x,_) -> Just $ if x == (1 :: Word8) then Sampled else NotSampled)
          . Text.decimal
{-# INLINE _Sampled #-}

freshContext :: (MonadIO m, MonadReader Env m) => m Context
freshContext = do
    prng <- asks envPRNG
    trid <- newTraceID prng
    spid <- newSpanID  prng
    return SimpleContext
        { ctxTraceID  = trid
        , ctxSpanID   = spid
        , _ctxSampled = Sampled
        , _ctxBaggage = mempty
        }

fromParent :: (MonadIO m, MonadReader Env m) => Reference Context -> m Context
fromParent p = do
    prng <- asks envPRNG
    spid <- newSpanID prng
    return SimpleContext
        { ctxTraceID  = ctxTraceID (refCtx p)
        , ctxSpanID   = spid
        , _ctxSampled = _ctxSampled (refCtx p)
        , _ctxBaggage = mempty
        }

spanE :: FinishedSpan Context -> Encoding
spanE s = pairs $
       pair "operation"  (text $ view spanOperation s)
    <> pair "start"      (utcTime $ view spanStart s)
    <> pair "duration"   (double . realToFrac $ view spanDuration s)
    <> pair "context"    (toEncoding $ view spanContext s)
    <> pair "references" (list refE . toList $ view spanRefs s)
    <> pair "tags"       (list tagE . toList $ view spanTags s)
    <> pair "logs"       (list logRecE . reverse $ view spanLogs s)

refE :: Reference Context -> Encoding
refE (ChildOf     ctx) = pairs . pair "child_of"     . toEncoding $ ctx
refE (FollowsFrom ctx) = pairs . pair "follows_from" . toEncoding $ ctx

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

logRecE :: LogRecord -> Encoding
logRecE r = pairs $
       pair "time"   (utcTime (view logTime r))
    <> pair "fields" (list logFieldE . toList $ view logFields r)

logFieldE :: LogField -> Encoding
logFieldE f = pairs . pair (logFieldLabel f) $ case f of
    Event      x -> text x
    Message    x -> text x
    Stack      x -> string . prettyCallStack $ x
    ErrKind    x -> text x
    LogField _ x -> string (show x)

makeLenses ''SimpleContext
