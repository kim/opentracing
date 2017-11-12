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

module OpenTracing.Standard
    ( Sampled(..)

    , StdContext
    , ctxTraceID
    , ctxSpanID
    , ctxSampled
    , ctxBaggage

    , Env
    , newEnv

    , stdTracer
    , stdReporter
    )
where

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
import qualified Data.Text.Read             as Text
import           Data.Word
import           GHC.Generics               (Generic)
import           GHC.Stack                  (prettyCallStack)
import           OpenTracing.Log
import           OpenTracing.Propagation
import           OpenTracing.Sampling       (Sampler (runSampler))
import           OpenTracing.Span
import           OpenTracing.Types
import           Prelude                    hiding (putStrLn)
import           System.Random.MWC


type SpanID = Word64

data StdContext = StdContext
    { ctxTraceID  :: TraceID
    , ctxSpanID   :: SpanID
    , ctxSampled' :: Sampled
    , _ctxBaggage :: HashMap Text Text
    } deriving (Eq, Show, Generic)

instance Hashable StdContext

instance HasSampled StdContext where
    ctxSampled = lens ctxSampled' (\s a -> s { ctxSampled' = a })

instance ToJSON StdContext where
    toEncoding StdContext{..} = pairs $
           "trace_id" .= view hexText ctxTraceID
        <> "span_id"  .= view hexText ctxSpanID
        <> "sampled"  .= ctxSampled'
        <> "baggage"  .= _ctxBaggage

    toJSON StdContext{..} = object
        [ "trace_id" .= view hexText ctxTraceID
        , "span_id"  .= view hexText ctxSpanID
        , "sampled"  .= ctxSampled'
        , "baggage"  .= _ctxBaggage
        ]


instance AsCarrier TextMap StdContext StdContext where
    _Carrier = prism' fromCtx toCtx
      where
        fromCtx c@StdContext{..} = TextMap . HashMap.fromList $
              ("ot-tracer-traceid", view hexText ctxTraceID)
            : ("ot-tracer-spanid" , view hexText ctxSpanID)
            : ("ot-tracer-sampled", view (ctxSampled . re _Sampled) c)
            : map (over _1 ("ot-baggage-" <>)) (HashMap.toList _ctxBaggage)

        toCtx (TextMap m) = StdContext
            <$> (HashMap.lookup "ot-tracer-traceid" m >>= preview _Hex . knownHex)
            <*> (HashMap.lookup "ot-tracer-spanid"  m >>= preview _Hex . knownHex)
            <*> (HashMap.lookup "ot-tracer-sampled" m >>= preview _Sampled)
            <*> pure (HashMap.filterWithKey (\k _ -> "ot-baggage-" `isPrefixOf` k) m)


instance AsCarrier HttpHeaders StdContext StdContext where
    _Carrier = prism' fromCtx toCtx
      where
        fromCtx
            = HttpHeaders
            . map (bimap (CI.mk . encodeUtf8) encodeUtf8)
            . HashMap.toList
            . fromTextMap
            . (review _Carrier :: StdContext -> TextMap StdContext)

        toCtx
            = (preview _Carrier :: TextMap StdContext -> Maybe StdContext)
            . TextMap
            . HashMap.fromList
            . map (bimap (toLower . decodeUtf8 . CI.original) decodeUtf8)
            . fromHttpHeaders


data Env = Env
    { envPRNG     :: GenIO
    , _envSampler :: Sampler
    }

newEnv :: MonadIO m => Sampler -> m Env
newEnv samp = do
    prng <- liftIO createSystemRandom
    return Env { envPRNG = prng, _envSampler = samp }

stdTracer :: MonadIO m => Env -> SpanOpts StdContext -> m (Span StdContext)
stdTracer r = flip runReaderT r . start

stdReporter :: MonadIO m => FinishedSpan StdContext -> m ()
stdReporter f = liftIO $ report f

--------------------------------------------------------------------------------
-- Internal

start :: (MonadIO m, MonadReader Env m) => SpanOpts StdContext -> m (Span StdContext)
start so@SpanOpts{spanOptOperation,spanOptRefs,spanOptTags} = do
    ctx <- do
        p <- findParent <$> liftIO (freezeRefs spanOptRefs)
        case p of
            Nothing -> freshContext so
            Just p' -> fromParent   (refCtx p')
    newSpan ctx spanOptOperation spanOptRefs spanOptTags

report :: FinishedSpan StdContext -> IO ()
report = putStrLn . encodingToLazyByteString . spanE

newTraceID :: (MonadIO m, MonadReader Env m) => m TraceID
newTraceID = asks envPRNG >>= fmap (TraceID Nothing) . liftIO . uniform

newSpanID :: (MonadIO m, MonadReader Env m) => m SpanID
newSpanID = asks envPRNG >>= liftIO . uniform

_Sampled :: Prism' Text Sampled
_Sampled = prism' enc dec
    where
      enc = \case Sampled -> "1"
                  _       -> "0"

      dec = either (const Nothing) id
          . fmap (\(x,_) -> Just $ if x == (1 :: Word8) then Sampled else NotSampled)
          . Text.decimal
{-# INLINE _Sampled #-}

freshContext
    :: ( MonadIO         m
       , MonadReader Env m
       )
    => SpanOpts StdContext
    -> m StdContext
freshContext SpanOpts{spanOptOperation,spanOptSampled} = do
    trid <- newTraceID
    spid <- newSpanID
    smpl <- asks _envSampler

    sampled' <- case spanOptSampled of
        Nothing -> view sampled <$> (runSampler smpl) trid spanOptOperation
        Just s  -> pure s

    return StdContext
        { ctxTraceID  = trid
        , ctxSpanID   = spid
        , ctxSampled' = sampled'
        , _ctxBaggage = mempty
        }

fromParent
    :: ( MonadIO         m
       , MonadReader Env m
       )
    => StdContext
    -> m StdContext
fromParent p = do
    spid <- newSpanID
    return StdContext
        { ctxTraceID  = ctxTraceID p
        , ctxSpanID   = spid
        , ctxSampled' = view ctxSampled p
        , _ctxBaggage = mempty
        }

spanE :: FinishedSpan StdContext -> Encoding
spanE s = pairs $
       pair "operation"  (text $ view spanOperation s)
    <> pair "start"      (utcTime $ view spanStart s)
    <> pair "duration"   (double . realToFrac $ view spanDuration s)
    <> pair "context"    (toEncoding $ view spanContext s)
    <> pair "references" (list refE . toList $ view spanRefs s)
    <> pair "tags"       (toEncoding $ view spanTags s)
    <> pair "logs"       (list logRecE . reverse $ view spanLogs s)

refE :: Reference StdContext -> Encoding
refE (ChildOf     ctx) = pairs . pair "child_of"     . toEncoding $ ctx
refE (FollowsFrom ctx) = pairs . pair "follows_from" . toEncoding $ ctx

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
    ErrObj     x -> string . show $ x
    LogField _ x -> string . show $ x

makeLenses ''StdContext
