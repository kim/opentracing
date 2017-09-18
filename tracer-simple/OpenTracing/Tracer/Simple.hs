{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module OpenTracing.Tracer.Simple
    ( Sampled(..)

    , Context
    , ctxTraceID
    , ctxSpanID
    , ctxSampled
    , ctxBaggage

    , Tracer
    , Env(envPRNG, envReporterConfig)
    , newEnv

    , PRNG
    , TraceID
    , SpanID

    , newPRNG

    , traceStart

    , runTracer
    )
where

import           Codec.Serialise
import           Control.Lens                hiding (Context, (.=))
import           Control.Monad.Catch
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Aeson.Encoding
import qualified Data.CaseInsensitive        as CI
import           Data.Hashable
import           Data.HashMap.Strict         (HashMap, fromList, toList)
import qualified Data.HashMap.Strict         as HashMap
import           Data.HashSet                (HashSet)
import qualified Data.HashSet                as HashSet
import           Data.Monoid
import           Data.Set                    (Set)
import           Data.Text                   (Text, isPrefixOf, toLower)
import           Data.Text.Encoding          (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy.Builder      as TB
import qualified Data.Text.Lazy.Builder.Int  as TB
import qualified Data.Text.Read              as Text
import           Data.Word
import           GHC.Generics                (Generic)
import           OpenTracing.Reporter.Config
import           OpenTracing.Types
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


type Context = SimpleContext

instance AsCarrier (TextMap SimpleContext) SimpleContext where
    _Carrier = prism' fromCtx toCtx
      where
        fromCtx SimpleContext{..} = TextMap $
            (fromList
                [ ("ot-tracer-traceid", review _ID ctxTraceID)
                , ("ot-tracer-spanid" , review _ID ctxSpanID)
                , ("ot-tracer-sampled", review _Sampled _ctxSampled)
                ])
            <> (fromList (map (\x -> over _1 ("ot-baggage-" <>) x) (toList _ctxBaggage)))

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
            . toList
            . fromTextMap
            . (review _Carrier :: Context -> TextMap Context)

        toCtx
            = (preview _Carrier :: TextMap Context -> Maybe Context)
            . TextMap
            . fromList
            . map (bimap (toLower . decodeUtf8 . CI.original) decodeUtf8)
            . fromHttpHeaders

instance AsCarrier (Binary SimpleContext) SimpleContext where
    _Carrier = prism' fromCtx toCtx
      where
        fromCtx = Binary . serialise
        toCtx   = either (const Nothing) pure . deserialiseOrFail . fromBinary


data Env = Env
    { envPRNG           :: PRNG
    , envReporterConfig :: Config
    }

newEnv :: MonadIO m => ConfigSource -> m Env
newEnv cs = Env <$> newPRNG <*> loadConfig cs

newtype Tracer m a = Tracer (ReaderT Env m a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadTrans
             , MonadReader Env
             , MonadCatch
             , MonadMask
             , MonadThrow
             )

traceStart
    :: MonadIO m
    => Text
    -> HashSet (Reference Context)
    -> Set Tag
    -> Tracer m (Span Context)
traceStart name refs tags = do
    ctx <- case HashSet.toList refs of
               []    -> freshContext
               (p:_) -> fromParent p
    newSpan ctx name refs tags

runTracer :: MonadIO m => Env -> Tracer m a -> m a
runTracer e (Tracer m) = runReaderT m e

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
        , _ctxSampled = NotSampled
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

makeLenses ''SimpleContext
