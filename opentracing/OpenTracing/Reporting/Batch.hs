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
    , boptAtCapacity
    , boptBatchSize
    , boptErrorLog
    , boptQueueSize
    , boptReporter
    , boptTimeoutSec

    , AtCapacity (..)

    , defaultErrorLog

    , BatchEnv
    , newBatchEnv
    , closeBatchEnv

    , batchReporter
    )
where

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception        (AsyncException (ThreadKilled))
import           Control.Exception.Safe
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.ByteString.Builder
import           Data.Time                (NominalDiffTime)
import           Data.Word
import           Numeric.Natural          (Natural)
import           OpenTracing.Span
import           OpenTracing.Time
import           System.IO                (stderr)
import           System.Timeout

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
    , _boptQueueSize  :: Natural
    -- ^ Size of the queue holding batched spans. Default 1000
    , _boptAtCapacity :: AtCapacity
    -- ^ What to do when the queue is at capacity. Default: Drop
    }

-- | Policy to apply to new spans when the internal queue is at capacity.
data AtCapacity = Drop | Block

-- | Default batch options which can be overridden via lenses.
batchOptions :: ([FinishedSpan] -> IO ()) -> BatchOptions
batchOptions f = BatchOptions
    { _boptBatchSize  = 100
    , _boptTimeoutSec = 5
    , _boptReporter   = f
    , _boptErrorLog   = defaultErrorLog
    , _boptQueueSize  = 1000
    , _boptAtCapacity = Drop
    }

-- | An error logging function which prints to stderr.
defaultErrorLog :: Builder -> IO ()
defaultErrorLog = hPutBuilder stderr

makeLenses ''BatchOptions

-- | The environment of a batch reporter.
data BatchEnv = BatchEnv
    { envQ   :: TBQueue FinishedSpan
    -- ^ The queue of spans to be reported
    , envRep :: Async ()
    -- ^ Asynchronous consumer of the queue
    , envCap :: AtCapacity
    -- ^ Policy to apply when the queue is at capacity
    , envLog :: Builder -> IO ()
    -- ^ Where to report errors
    }

-- | Create a new batch environment
newBatchEnv :: BatchOptions -> IO BatchEnv
newBatchEnv opt = do
    q <- newTBQueueIO (_boptQueueSize opt)
    c <- consumer opt q
    pure BatchEnv
        { envQ = q
        , envRep = c
        , envCap = _boptAtCapacity opt
        , envLog = _boptErrorLog opt
        }

-- | Close a batch reporter, stop consuming any new spans. Any
-- spans in the queue will be drained.
closeBatchEnv :: BatchEnv -> IO ()
closeBatchEnv = cancel . envRep

-- | An implementation of `OpenTracing.Tracer.tracerReport` that batches the
-- finished spans for transimission to their destination.
--
-- If the underlying queue is currently at capacity, the behaviour depends on
-- the setting of `boptAtCapacity`: if the value is `Drop`, `fspan` is dropped,
-- otherwise, if the value is `Block`, the reporter will block until the queue
-- has enough space to accept the span.
--
--  In either case, a log record is emitted.
batchReporter :: MonadIO m => BatchEnv -> FinishedSpan -> m ()
batchReporter BatchEnv{envCap = Block, envQ, envLog} fspan = liftIO $ do
    full <- atomically $ isFullTBQueue envQ
    when full $
        envLog "Queue at capacity, enqueueing span may block\n"
    atomically $ writeTBQueue envQ fspan

batchReporter BatchEnv{envCap = Drop, envQ, envLog} fspan = liftIO $ do
    full <- atomically $ do
        full <- isFullTBQueue envQ
        unless full $
            writeTBQueue envQ fspan
        pure full
    when full $
        envLog "Queue at capacity, span was dropped\n"

consumer :: BatchOptions -> TBQueue FinishedSpan -> IO (Async ())
consumer opt@BatchOptions{..} q = async . forever $ do
    xs <- popBlocking
    go False xs
  where
    popBlocking = atomically $ do
        x <- readTBQueue q
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


pop :: Word16 -> TBQueue a -> STM [a]
pop 0 _ = pure []
pop n q = do
    v <- tryReadTBQueue q
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
