{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE ViewPatterns          #-}

module OpenTracing.Zipkin
    ( ZipkinContext
    , ctxTraceID
    , ctxSpanID
    , ctxParentSpanID
    , ctxFlags
    , ctxBaggage

    , Flag(..)
    , hasFlag

    , Env(envPRNG)
    , envTraceID128bit
    , envSampler
    , newEnv

    , zipkinTracer
    )
where

import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import qualified Data.CaseInsensitive       as CI
import           Data.Hashable              (Hashable)
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HashMap
import           Data.HashSet               (HashSet)
import qualified Data.HashSet               as HashSet
import           Data.Maybe
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
import           OpenTracing.Propagation
import           OpenTracing.Sampling       (Sampler (runSampler))
import           OpenTracing.Span           hiding (Sampled)
import qualified OpenTracing.Span           as Span
import           OpenTracing.Types
import           System.Random.MWC


type SpanID  = Word64

data Flag
    = Debug
    | SamplingSet
    | Sampled
    | IsRoot
    deriving (Eq, Show, Generic, Ord)

instance Hashable Flag


data ZipkinContext = ZipkinContext
    { ctxTraceID      :: TraceID
    , ctxSpanID       :: SpanID
    , ctxParentSpanID :: Maybe SpanID
    , _ctxFlags       :: HashSet Flag
    , _ctxBaggage     :: HashMap Text Text
    } deriving (Eq, Show, Generic)

instance Hashable  ZipkinContext

instance HasSampled ZipkinContext where
    ctxSampled = lens sa sbt
      where
        sa s | HashSet.member Sampled (_ctxFlags s) = Span.Sampled
             | otherwise                            = Span.NotSampled

        sbt s Span.Sampled    = s { _ctxFlags = HashSet.insert Sampled (_ctxFlags s) }
        sbt s Span.NotSampled = s { _ctxFlags = HashSet.delete Sampled (_ctxFlags s) }


instance AsCarrier TextMap ZipkinContext ZipkinContext where
    _Carrier = prism' fromCtx toCtx
      where
        fromCtx ZipkinContext{..} = TextMap . HashMap.fromList . catMaybes $
              Just ("x-b3-traceid", view (re _Hex . to unHex) ctxTraceID)
            : Just ("x-b3-spanid" , view (re _Hex . to unHex) ctxSpanID)
            : fmap (("x-b3-parentspanid",) . view (re _Hex . to unHex)) ctxParentSpanID
            : Just ("x-b3-sampled", if HashSet.member Sampled _ctxFlags then "true" else "false")
            : Just ("x-b3-flags"  , if HashSet.member Debug   _ctxFlags then "1"    else "0")
            : map (Just . over _1 ("ot-baggage-" <>)) (HashMap.toList _ctxBaggage)

        toCtx (TextMap m) = ZipkinContext
            <$> (HashMap.lookup "x-b3-traceid" m >>= preview _Hex . Hex)
            <*> (HashMap.lookup "x-b3-spanid"  m >>= preview _Hex . Hex)
            <*> (Just $ HashMap.lookup "x-b3-parentspanid" m >>= preview _Hex . Hex)
            <*> pure (HashSet.fromList $ catMaybes
                    [ HashMap.lookup "x-b3-sampled" m
                        >>= \case "true" -> Just Sampled
                                  _      -> Nothing
                    , HashMap.lookup "x-b3-flags" m
                        >>= \case "1" -> Just Debug
                                  _   -> Nothing
                    ]
                )
            <*> pure (HashMap.filterWithKey (\k _ -> "ot-baggage-" `isPrefixOf` k) m)


instance AsCarrier HttpHeaders ZipkinContext ZipkinContext where
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

hasFlag :: Flag -> ZipkinContext -> Bool
hasFlag f = HashSet.member f . _ctxFlags


data Env = Env
    { envPRNG           :: GenIO
    , _envTraceID128bit :: Bool
    , _envSampler       :: Sampler
    }

newEnv :: MonadIO m => Sampler -> m Env
newEnv samp = do
    prng <- liftIO createSystemRandom
    return Env
        { envPRNG           = prng
        , _envTraceID128bit = True
        , _envSampler       = samp
        }

instance MonadIO m => MonadTrace ZipkinContext (ReaderT Env m) where
    traceStart = start

zipkinTracer :: Env -> Interpret (MonadTrace ZipkinContext) MonadIO
zipkinTracer r = Interpret $ \m -> runReaderT m r

start :: (MonadIO m, MonadReader Env m) => SpanOpts ZipkinContext -> m (Span ZipkinContext)
start so@SpanOpts{..} = do
    ctx <- case spanOptRefs of
               []    -> freshContext so
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
    return TraceID { traceIdHi = hi, traceIdLo = lo }

newSpanID :: (MonadIO m, MonadReader Env m) => m SpanID
newSpanID = ask >>= liftIO . uniform . envPRNG

freshContext :: (MonadIO m, MonadReader Env m) => SpanOpts ZipkinContext -> m ZipkinContext
freshContext SpanOpts{spanOptOperation,spanOptSampled} = do
    trid <- newTraceID
    spid <- newSpanID
    smpl <- asks _envSampler

    sampled <- case spanOptSampled of
        Nothing              -> (runSampler smpl) trid spanOptOperation
        Just Span.Sampled    -> pure True
        Just Span.NotSampled -> pure False

    return ZipkinContext
        { ctxTraceID      = trid
        , ctxSpanID       = spid
        , ctxParentSpanID = Nothing
        , _ctxFlags       = if sampled then HashSet.singleton Sampled else mempty
        , _ctxBaggage     = mempty
        }

fromParent :: (MonadIO m, MonadReader Env m) => Reference ZipkinContext -> m ZipkinContext
fromParent (refCtx -> p) = do
    spid <- newSpanID
    return ZipkinContext
        { ctxTraceID      = ctxTraceID p
        , ctxSpanID       = spid
        , ctxParentSpanID = Just $ ctxSpanID p
        , _ctxFlags       = _ctxFlags p
        , _ctxBaggage     = mempty
        }

makeLenses ''ZipkinContext
makeLenses ''Env
