{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE ViewPatterns               #-}

module Zipkin.Tracer
    ( Context
    , ctxTraceID
    , ctxSpanID
    , ctxParentSpanID
    , ctxSampled
    , ctxFlags
    , ctxBaggage

    , Flag(..)

    , Tracer
    , Env(envPRNG, envReporterConfig)
    , envTraceID128bit
    , newEnv

    , PRNG
    , TraceID
    , SpanID
    , _ID

    , newPRNG

    , traceStart

    , runTracer
    )
where

import           Codec.Serialise
import           Control.Lens                hiding (Context)
import           Control.Monad               (mzero)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Bool                   (bool)
import qualified Data.CaseInsensitive        as CI
import           Data.Hashable               (Hashable)
import           Data.HashMap.Strict         (HashMap, fromList, toList)
import qualified Data.HashMap.Strict         as HashMap
import           Data.HashSet                (HashSet)
import qualified Data.HashSet                as HashSet
import           Data.Monoid
import           Data.Set                    (Set)
import           Data.Text                   (Text, isPrefixOf, toLower)
import qualified Data.Text                   as Text
import           Data.Text.Encoding          (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy.Builder      as TB
import qualified Data.Text.Lazy.Builder.Int  as TB
import qualified Data.Text.Read              as TR
import           Data.Word
import           GHC.Generics                (Generic)
import           OpenTracing.Reporter.Config
import           OpenTracing.Types
import           System.Random.MWC


type TraceID = ZTraceID
type SpanID  = Word64

data ZTraceID = ZTraceID
    { ztHi :: Maybe Word64
    , ztLo :: Word64
    } deriving (Eq, Show, Generic)

instance Hashable  ZTraceID
instance Serialise ZTraceID


data Flag
    = Debug
    | SamplingSet
    | Sampled
    | IsRoot
    deriving (Eq, Show, Generic, Ord)

instance Hashable  Flag
instance Serialise Flag


type Context = ZipkinContext

data ZipkinContext = ZipkinContext
    { ctxTraceID      :: TraceID
    , ctxSpanID       :: SpanID
    , ctxParentSpanID :: Maybe SpanID
    --, ctxIsOwner      :: Bool
    , _ctxSampled     :: Bool
    , _ctxFlags       :: HashSet Flag
    , _ctxBaggage     :: HashMap Text Text
    } deriving (Eq, Show, Generic)

instance Hashable  ZipkinContext
instance Serialise ZipkinContext

instance AsCarrier (TextMap ZipkinContext) ZipkinContext where
    _Carrier = prism' fromCtx toCtx
      where
        fromCtx ZipkinContext{..} = TextMap . fromList . (>>= maybe mzero return) $
              Just ("x-b3-traceid", review _ID ctxTraceID)
            : Just ("x-b3-spanid" , review _ID ctxSpanID)
            : fmap (("x-b3-parentspanid",) . review _ID) ctxParentSpanID
            : Just ("x-b3-sampled", if _ctxSampled then "true" else "false")
            : Just ("x-b3-flags"  , if HashSet.member Debug _ctxFlags then "1" else "0")
            : map (Just . over _1 ("ot-baggage-" <>)) (toList _ctxBaggage)

        toCtx (TextMap m) = ZipkinContext
            <$> (HashMap.lookup "x-b3-traceid" m >>= preview _ID)
            <*> (HashMap.lookup "x-b3-spanid"  m >>= preview _ID)
            <*> (Just $ HashMap.lookup "x-b3-parentspanid" m >>= preview _ID)
            <*> (HashMap.lookup "x-b3-sampled" m >>= preview _Sampled)
            <*> undefined
            <*> pure (HashMap.filterWithKey (\k _ -> "ot-baggage-" `isPrefixOf` k) m)


instance AsCarrier (HttpHeaders ZipkinContext) ZipkinContext where
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

instance AsCarrier (Binary ZipkinContext) ZipkinContext where
    _Carrier = prism' fromCtx toCtx
      where
        fromCtx = Binary . serialise
        toCtx   = either (const Nothing) pure . deserialiseOrFail . fromBinary


newtype PRNG = PRNG { unPRNG :: GenIO }


data Env = Env
    { envPRNG           :: PRNG
    , envReporterConfig :: Config
    , _envTraceID128bit :: Bool
    }

newEnv :: MonadIO m => ConfigSource -> m Env
newEnv cs = Env <$> newPRNG <*> loadConfig cs <*> pure True

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

newTraceID :: (MonadIO m, MonadReader Env m) => m TraceID
newTraceID = do
    env <- ask
    hi  <- if _envTraceID128bit env then
               Just <$> liftIO (uniform (prng env))
           else
               pure Nothing
    lo  <- liftIO $ uniform (prng env)
    return ZTraceID { ztHi = hi, ztLo = lo }
  where
    prng = unPRNG . envPRNG

newSpanID :: (MonadIO m, MonadReader Env m) => m SpanID
newSpanID = ask >>= liftIO . uniform . unPRNG . envPRNG

_ID :: AsID a => Prism' Text a
_ID = _ID'

class AsID a where
    _ID' :: Prism' Text a

instance AsID TraceID where
    _ID' = prism' enc dec
      where
        enc (ZTraceID hi lo) = maybe mempty (review _ID') hi <> review _ID' lo
        dec t = case Text.splitAt 16 t of
                    ("", lo) -> ZTraceID Nothing <$> preview _ID' lo
                    (hi, lo) -> ZTraceID <$> Just (preview _ID' hi) <*> preview _ID' lo
    {-# INLINE _ID' #-}

instance AsID SpanID where
    _ID' = prism' enc dec
      where
        enc = view strict . TB.toLazyText . TB.hexadecimal
        dec = either (const Nothing) (pure . fst) . TR.hexadecimal
    {-# INLINE _ID' #-}

_Sampled :: Prism' Text Bool
_Sampled = prism' (bool "false" "true") (Just . (== "true"))

freshContext :: (MonadIO m, MonadReader Env m) => m Context
freshContext = do
    trid <- newTraceID
    spid <- newSpanID
    return ZipkinContext
        { ctxTraceID      = trid
        , ctxSpanID       = spid
        , ctxParentSpanID = Nothing
        , _ctxSampled     = True
        , _ctxFlags       = mempty
        , _ctxBaggage     = mempty
        }

fromParent :: (MonadIO m, MonadReader Env m) => Reference Context -> m Context
fromParent (refCtx -> p) = do
    spid <- newSpanID
    return ZipkinContext
        { ctxTraceID      = ctxTraceID p
        , ctxSpanID       = spid
        , ctxParentSpanID = Just $ ctxSpanID p
        , _ctxSampled     = _ctxSampled p
        , _ctxFlags       = _ctxFlags p
        , _ctxBaggage     = mempty
        }

makeLenses ''ZipkinContext
makeLenses ''Env
