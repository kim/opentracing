{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE ViewPatterns          #-}

module OpenTracing.Zipkin
    ( ZipkinContext
    , ctxTraceID
    , ctxSpanID
    , ctxParentSpanID
    , ctxSampled
    , ctxFlags
    , ctxBaggage

    , Flag(..)

    , Env(envPRNG)
    , envTraceID128bit
    , newEnv

    , zipkinTracer

    , _ID
    )
where

import           Control.Lens
import           Control.Monad              (mzero)
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Bool                  (bool)
import qualified Data.CaseInsensitive       as CI
import           Data.Hashable              (Hashable)
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HashMap
import           Data.HashSet               (HashSet)
import qualified Data.HashSet               as HashSet
import           Data.Monoid
import           Data.Text                  (Text, isPrefixOf, toLower)
import qualified Data.Text                  as Text
import           Data.Text.Encoding         (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy.Builder     as TB
import qualified Data.Text.Lazy.Builder.Int as TB
import qualified Data.Text.Read             as TR
import           Data.Word
import           GHC.Generics               (Generic)
import           OpenTracing.Class
import           OpenTracing.Types
import           System.Random.MWC


type TraceID = ZTraceID
type SpanID  = Word64

data ZTraceID = ZTraceID
    { ztHi :: Maybe Word64
    , ztLo :: Word64
    } deriving (Eq, Show, Generic)

instance Hashable  ZTraceID


data Flag
    = Debug
    | SamplingSet
    | Sampled
    | IsRoot
    deriving (Eq, Show, Generic, Ord)

instance Hashable  Flag


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

instance AsCarrier (TextMap ZipkinContext) ZipkinContext where
    _Carrier = prism' fromCtx toCtx
      where
        fromCtx ZipkinContext{..} = TextMap . HashMap.fromList . (>>= maybe mzero return) $
              Just ("x-b3-traceid", review _ID ctxTraceID)
            : Just ("x-b3-spanid" , review _ID ctxSpanID)
            : fmap (("x-b3-parentspanid",) . review _ID) ctxParentSpanID
            : Just ("x-b3-sampled", if _ctxSampled then "true" else "false")
            : Just ("x-b3-flags"  , if HashSet.member Debug _ctxFlags then "1" else "0")
            : map (Just . over _1 ("ot-baggage-" <>)) (HashMap.toList _ctxBaggage)

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
            . HashMap.toList
            . fromTextMap
            . (review _Carrier :: ZipkinContext -> TextMap ZipkinContext)

        toCtx
            = (preview _Carrier :: TextMap ZipkinContext -> Maybe ZipkinContext)
            . TextMap
            . HashMap.fromList
            . map (bimap (toLower . decodeUtf8 . CI.original) decodeUtf8)
            . fromHttpHeaders


data Env = Env
    { envPRNG           :: GenIO
    , _envTraceID128bit :: Bool
    }

newEnv :: MonadIO m => m Env
newEnv = Env
    <$> liftIO createSystemRandom
    <*> pure True

instance MonadIO m => MonadTrace ZipkinContext (ReaderT Env m) where
    traceStart = start

zipkinTracer :: Env -> Interpret (MonadTrace ZipkinContext) MonadIO
zipkinTracer r = Interpret $ \m -> runReaderT m r

start :: (MonadIO m, MonadReader Env m) => SpanOpts ZipkinContext -> m (Span ZipkinContext)
start SpanOpts{..} = do
    ctx <- case spanOptRefs of
               []    -> freshContext
               (p:_) -> fromParent p
    newSpan ctx spanOptOperation spanOptRefs spanOptTags

newTraceID :: (MonadIO m, MonadReader Env m) => m TraceID
newTraceID = do
    Env{..} <- ask
    hi <- if _envTraceID128bit then
              Just <$> liftIO (uniform envPRNG)
          else
              pure Nothing
    lo <- liftIO $ uniform envPRNG
    return ZTraceID { ztHi = hi, ztLo = lo }

newSpanID :: (MonadIO m, MonadReader Env m) => m SpanID
newSpanID = ask >>= liftIO . uniform . envPRNG

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

freshContext :: (MonadIO m, MonadReader Env m) => m ZipkinContext
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

fromParent :: (MonadIO m, MonadReader Env m) => Reference ZipkinContext -> m ZipkinContext
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
