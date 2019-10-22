{-|
Module: OpenTracing.Reporting.Batch

This module provides a trace reporter that groups recorded spans into batches
before sending them to their destination in bulk.

-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}

module OpenTracing.Reporting.Batch
    ( BatchOptions
    , batchOptions
    , boptBatchSize
    , boptTimeoutSec
    , boptReporter
    , boptErrorLog

    , defaultErrorLog

    , BatchEnv
    , newBatchEnv
    , closeBatchEnv

    , batchReporter
    )
where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception        (AsyncException (ThreadKilled))
import Control.Exception.Safe
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString.Builder
import Data.Semigroup
import Data.Time                (NominalDiffTime)
import Data.Word
import OpenTracing.Span
import OpenTracing.Time
import System.IO                (stderr)
import System.Timeout

-- | Options available to construct a batch reporter. Default options are
-- available with `batchOptions`
data BatchOptions = BatchOptions
    { _boptBatchSize  :: Word16
    -- ^ The maximum number of elements to report in a batch. Default 100
    , _boptTimeoutSec :: Word
    -- ^ The maximum time (in seconds) to wait while reporting a batch before erroring.
    -- Default 5 seconds.
    , _boptReporter   :: [FinishedSpan] -> IO ()
    -- ^ The function to call with the batch of spans. Has an upper bound on size equal
    -- to _boptBatchSize. No default.
    , _boptErrorLog   :: Builder        -> IO ()
    -- ^ What to do with errors. Default print to stderr.
    }

-- | Default batch options which can be overloaded via lenses.
batchOptions :: ([FinishedSpan] -> IO ()) -> BatchOptions
batchOptions f = BatchOptions
    { _boptBatchSize  = 100
    , _boptTimeoutSec = 5
    , _boptReporter   = f
    , _boptErrorLog   = defaultErrorLog
    }

-- | An error logging function which prints to stderr.
defaultErrorLog :: Builder -> IO ()
defaultErrorLog = hPutBuilder stderr

makeLenses ''BatchOptions


-- | The environment of a batch reporter.
data BatchEnv = BatchEnv
    { envQ   :: TQueue FinishedSpan
    -- ^ The queue of spans to be reported
    , envRep :: Async ()
    -- ^ Asyncronous consumer of the queue
    }

-- | Create a new batch environment
newBatchEnv :: BatchOptions -> IO BatchEnv
newBatchEnv opt = do
    q <- newTQueueIO
    BatchEnv q <$> consumer opt q

-- | Close a batch reporter, stop consiming any new spans. Any
-- spans in the queue will be drained.
closeBatchEnv :: BatchEnv -> IO ()
closeBatchEnv = cancel . envRep

-- | An implementation of `OpenTracing.Tracer.tracerReport` that batches the finished
-- spans for transimission to their destination.
batchReporter :: MonadIO m => BatchEnv -> FinishedSpan -> m ()
batchReporter BatchEnv{envQ} = liftIO . atomically . writeTQueue envQ

consumer :: BatchOptions -> TQueue FinishedSpan -> IO (Async ())
consumer opt@BatchOptions{..} q = async . forever $ do
    xs <- popBlocking
    go False xs
  where
    popBlocking = atomically $ do
        x <- readTQueue q
        (x:) <$> pop (_boptBatchSize - 1) q

    popNonblock = atomically $ pop _boptBatchSize q

    go _     []    = pure ()
    go True  batch = report batch *> drain
    go False batch = withAsync (report batch) $ \a ->
        timedWait a `catchAsync` \case
            ThreadKilled -> do
                logErr opt ErrReporterCancelled
                timedWait a `finally` uninterruptibleCancel a
                drain
                throwM ThreadKilled

            e -> logErr opt (ErrReporterAsyncException e) *> throwM e

    report batch = _boptReporter batch `catchAny`
        (logErr opt . ErrReporterException)

    timedWait a = timeout timeoutMicros (wait a) >>= \case
        Nothing -> logErr opt ErrReporterTimeout
        _       -> pure ()

    drain = do
        logErr opt ErrReporterDraining
        popNonblock >>= go True

    timeoutMicros = micros @NominalDiffTime $ fromIntegral _boptTimeoutSec


pop :: Word16 -> TQueue a -> STM [a]
pop 0 _ = pure []
pop n q = do
    v <- tryReadTQueue q
    case v of
        Nothing -> pure []
        Just v' -> (v' :) <$> pop (n-1) q

data Err
    = ErrReporterException      SomeException
    | ErrReporterTimeout
    | ErrReporterCancelled
    | ErrReporterAsyncException AsyncException
    | ErrReporterDraining

logErr :: BatchOptions -> Err -> IO ()
logErr BatchOptions{_boptErrorLog=errlog} e = errlog $ msg e <> nl
  where
    sbs = shortByteString

    ebs :: Exception e => e -> Builder
    ebs = string8 . show

    msg = \case
        ErrReporterException      ex -> sbs "Reporter Error: " <> ebs ex
        ErrReporterTimeout           -> sbs "Reporter timed out!"
        ErrReporterCancelled         -> sbs "Batch reporter cancelled, shutting down gracefully"
        ErrReporterAsyncException ex -> sbs "Batch reporter received async exception: " <> ebs ex
        ErrReporterDraining          -> sbs "Draining batch reporter queue"

    nl = char8 '\n'
