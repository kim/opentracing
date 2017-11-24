{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}

module OpenTracing.Reporting
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
    w <- async $ go False
    handleAsync drain $ do
        wait w
        void . atomically $ peekTQueue q
  where
    go draining = do
        batch <- atomically $ pop _boptBatchSize q
        case batch of
            [] -> return ()
            xs -> do
                r <- timeout timeoutMicros $
                         _boptReporter xs
                             `catchAny` (logErr opt . ErrReporterException)
                case r of
                    Nothing -> logErr opt ErrReporterTimeout
                    Just () -> when draining $ go True

    drain ThreadKilled = logErr opt ErrReporterCancelled          *> go True
    drain e            = logErr opt (ErrReporterAsyncException e) *> throwM e

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

logErr :: BatchOptions -> Err -> IO ()
logErr BatchOptions{_boptErrorLog=errlog} e = errlog $ msg e <> nl
  where
    sbs = shortByteString

    ebs :: Exception e => e -> Builder
    ebs = string8 . show

    msg = \case
        ErrReporterException      ex -> sbs "Reporter Error: " <> ebs ex
        ErrReporterTimeout           -> sbs "Reporter timed out!"
        ErrReporterCancelled         -> sbs "Batch reporter cancelled"
        ErrReporterAsyncException ex -> sbs "Batch reporter received async exception: " <> ebs ex

    nl = char8 '\n'
