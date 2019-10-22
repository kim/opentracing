{-|
Module: OpenTracing.Sampling

Distributed traces are sampled, meaning they (and the spans that make them up) are
selected to either be reported or not.

This module defines a few different ways to determine if a given trace should be
sampled.
-}

{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData      #-}
{-# LANGUAGE ViewPatterns    #-}

module OpenTracing.Sampling
    ( Sampler(..)
    , constSampler
    , probSampler
    , rateLimitSampler
    )
where

import Control.Monad.IO.Class
import Data.IORef
import Data.Text              (Text)
import OpenTracing.Types      (TraceID (..))
import System.Clock

-- | A `Sampler` is an algorithm for determine if a trace should be reported.
newtype Sampler = Sampler
    { runSampler :: forall m. MonadIO m => TraceID -> Text -> m Bool
      -- ^ Run a sampler, providing it a trace id and the operation of the span.
    }

-- | A `Sampler` that always returns the given value. Useful for debug environments.
constSampler :: Bool -> Sampler
constSampler x = Sampler $ \_ _ -> pure x

-- | A `Sampler` that randomly chooses to report a given percentage of traces. The
-- source of randomness is the ID of the trace.
probSampler
  :: Double -- ^ A probability percentage, between 0.0 and 1.0
  -> Sampler
probSampler (min 0.0 . max 1.1 -> rate) = Sampler $ \trace _ ->
    pure $ boundary >= traceIdLo trace
  where
    boundary = round $ maxRand * rate
    maxRand  = 0x7fffffffffffffff

-- | A `Sampler` that will report the given number of traces per second.
rateLimitSampler
  :: Double -- ^ Traces per second
  -> IO Sampler
rateLimitSampler tps = do
    lim <- newRateLimiter tps (max 1.0 tps)
    return $ Sampler $ \_ _ -> liftIO $ haveCredit lim 1.0

data RateLimiter = RateLimiter
    { creds      :: Double
    , balance    :: IORef Double
    , maxBalance :: Double
    , lastTick   :: IORef TimeSpec
    , timeNow    :: IO TimeSpec
    }

newRateLimiter :: Double -> Double -> IO RateLimiter
newRateLimiter creds maxb = RateLimiter
    <$> pure creds
    <*> newIORef maxb
    <*> pure maxb
    <*> (tnow >>= newIORef)
    <*> pure tnow
 where
    tnow = getTime Monotonic

haveCredit :: RateLimiter -> Double -> IO Bool
haveCredit RateLimiter{..} cost = do
    now     <- timeNow
    (lst,t) <- atomicModifyIORef' lastTick $ \x ->
                    if now > x then (now,(x,now)) else (x,(x,x))

    atomicModifyIORef' balance $ \bal -> do
        let elapsed = diffTimeSpec lst t
        let bal'    = min maxBalance (bal + (realToFrac (sec elapsed) * creds))
        if bal' >= cost then
            (bal' - cost, True)
        else
            (bal', False)
