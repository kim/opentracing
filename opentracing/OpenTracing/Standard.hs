{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TemplateHaskell       #-}

module OpenTracing.Standard
    ( StdEnv
    , newStdEnv
    , envTraceID128bit
    , envSampler

    , stdTracer
    , stdReporter
    )
where

import Control.Lens                 hiding (Context, (.=))
import Control.Monad.Reader
import Data.Monoid
import Data.Word
import OpenTracing.Reporting.Stdio (stdoutReporter)
import OpenTracing.Sampling         (Sampler (runSampler))
import OpenTracing.Span
import OpenTracing.Types
import Prelude                      hiding (putStrLn)
import System.Random.MWC


data StdEnv = StdEnv
    { envPRNG           :: GenIO
    , _envSampler       :: Sampler
    , _envTraceID128bit :: Bool
    }

newStdEnv :: MonadIO m => Sampler -> m StdEnv
newStdEnv samp = do
    prng <- liftIO createSystemRandom
    return StdEnv { envPRNG = prng, _envSampler = samp, _envTraceID128bit = True }

makeLenses ''StdEnv

stdTracer :: MonadIO m => StdEnv -> SpanOpts -> m Span
stdTracer r = flip runReaderT r . start

stdReporter :: MonadIO m => FinishedSpan -> m ()
stdReporter = stdoutReporter

--------------------------------------------------------------------------------
-- Internal

start :: (MonadIO m, MonadReader StdEnv m) => SpanOpts -> m Span
start so@SpanOpts{spanOptOperation,spanOptRefs,spanOptTags} = do
    ctx <- do
        p <- findParent <$> liftIO (freezeRefs spanOptRefs)
        case p of
            Nothing -> freshContext so
            Just p' -> fromParent   (refCtx p')
    newSpan ctx spanOptOperation spanOptRefs spanOptTags

newTraceID :: (MonadIO m, MonadReader StdEnv m) => m TraceID
newTraceID = do
    StdEnv{..} <- ask
    hi <- if _envTraceID128bit then
              Just <$> liftIO (uniform envPRNG)
          else
              pure Nothing
    lo <- liftIO $ uniform envPRNG
    return TraceID { traceIdHi = hi, traceIdLo = lo }

newSpanID :: (MonadIO m, MonadReader StdEnv m) => m Word64
newSpanID = asks envPRNG >>= liftIO . uniform

freshContext
    :: ( MonadIO            m
       , MonadReader StdEnv m
       )
    => SpanOpts
    -> m SpanContext
freshContext SpanOpts{spanOptOperation,spanOptSampled} = do
    trid <- newTraceID
    spid <- newSpanID
    smpl <- view envSampler

    sampled' <- case spanOptSampled of
        Nothing -> view _IsSampled <$> runSampler smpl trid spanOptOperation
        Just s  -> pure s

    return SpanContext
        { ctxTraceID      = trid
        , ctxSpanID       = spid
        , ctxParentSpanID = Nothing
        , _ctxSampled     = sampled'
        , _ctxBaggage     = mempty
        }

fromParent
    :: ( MonadIO            m
       , MonadReader StdEnv m
       )
    => SpanContext
    -> m SpanContext
fromParent p = do
    spid <- newSpanID
    return SpanContext
        { ctxTraceID      = ctxTraceID p
        , ctxSpanID       = spid
        , ctxParentSpanID = Just (ctxSpanID p)
        , _ctxSampled     = view ctxSampled p
        , _ctxBaggage     = view ctxBaggage p
        }
