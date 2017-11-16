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

module OpenTracing.Zipkin
    ( Env(envPRNG)
    , envTraceID128bit
    , envSampler
    , newEnv

    , zipkinTracer
    )
where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Word
import OpenTracing.Sampling   (Sampler (runSampler))
import OpenTracing.Span
import OpenTracing.Types
import System.Random.MWC


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

zipkinTracer :: MonadIO m => Env -> SpanOpts -> m Span
zipkinTracer r = flip runReaderT r . start

start :: (MonadIO m, MonadReader Env m) => SpanOpts -> m Span
start so@SpanOpts{spanOptOperation,spanOptRefs,spanOptTags} = do
    ctx <- do
        p <- findParent <$> liftIO (freezeRefs spanOptRefs)
        case p of
            Nothing -> freshContext so
            Just p' -> fromParent   (refCtx p')
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

newSpanID :: (MonadIO m, MonadReader Env m) => m Word64
newSpanID = ask >>= liftIO . uniform . envPRNG

freshContext :: (MonadIO m, MonadReader Env m) => SpanOpts -> m SpanContext
freshContext SpanOpts{spanOptOperation,spanOptSampled} = do
    trid <- newTraceID
    spid <- newSpanID
    smpl <- asks _envSampler

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

fromParent :: (MonadIO m, MonadReader Env m) => SpanContext -> m SpanContext
fromParent p = do
    spid <- newSpanID
    return SpanContext
        { ctxTraceID      = ctxTraceID p
        , ctxSpanID       = spid
        , ctxParentSpanID = Just (ctxSpanID p)
        , _ctxSampled     = view ctxSampled p
        , _ctxBaggage     = view ctxBaggage p
        }

makeLenses ''Env
