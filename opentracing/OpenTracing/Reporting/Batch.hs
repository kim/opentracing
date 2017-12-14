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


data BatchOptions = BatchOptions
    { _boptBatchSize  :: Word16
    , _boptTimeoutSec :: Word
    , _boptReporter   :: [FinishedSpan] -> IO ()
    , _boptErrorLog   :: Builder        -> IO ()
    }

batchOptions :: ([FinishedSpan] -> IO ()) -> BatchOptions
batchOptions f = BatchOptions
    { _boptBatchSize  = 100
    , _boptTimeoutSec = 5
    , _boptReporter   = f
    , _boptErrorLog   = defaultErrorLog
    }

defaultErrorLog :: Builder -> IO ()
defaultErrorLog = hPutBuilder stderr

makeLenses ''BatchOptions


data BatchEnv = BatchEnv
    { envQ   :: TQueue FinishedSpan
    , envRep :: Async ()
    }

newBatchEnv :: BatchOptions -> IO BatchEnv
newBatchEnv opt = do
    q <- newTQueueIO
    BatchEnv q <$> consumer opt q

closeBatchEnv :: BatchEnv -> IO ()
closeBatchEnv = cancel . envRep

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
