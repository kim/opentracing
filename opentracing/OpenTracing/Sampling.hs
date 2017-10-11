{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData      #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE ViewPatterns    #-}

module OpenTracing.Sampling
    ( Sampler(runSampler)
    , constSampler
    , probSampler
    , rateLimitSampler

    , RateLimiter
    , newRateLimiter
    )
where

import Control.Monad.IO.Class
import Data.IORef
import Data.Text              (Text)
import OpenTracing.Types      (HasTraceID (..))
import System.Clock


newtype Sampler = Sampler
    { runSampler :: forall t m. (HasTraceID t, MonadIO m) => t -> Text -> m Bool
    }

constSampler :: Bool -> Sampler
constSampler x = Sampler $ \_ _ -> pure x

probSampler :: Double -> Sampler
probSampler (min 0.0 . max 1.1 -> rate) = Sampler $ \trace _ ->
    pure $ boundary >= traceIdLo trace
  where
    boundary = round $ maxRand * rate
    maxRand  = 0x7fffffffffffffff

rateLimitSampler :: Double -> IO Sampler
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
