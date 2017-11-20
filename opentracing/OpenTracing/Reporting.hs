{-# LANGUAGE NamedFieldPuns #-}

module OpenTracing.Reporting
    ( BatchEnv
    , newBatchEnv
    , closeBatchEnv

    , batchReporter
    )
where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception        (AsyncException (ThreadKilled))
import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import OpenTracing.Span


data BatchEnv = BatchEnv
    { envQ   :: TQueue FinishedSpan
    , envRep :: Async ()
    }

newBatchEnv :: Int -> ([FinishedSpan] -> IO ()) -> IO BatchEnv
newBatchEnv siz f = do
    q <- newTQueueIO
    BatchEnv q <$> consumer siz q f

closeBatchEnv :: BatchEnv -> IO ()
closeBatchEnv = cancel . envRep

batchReporter :: MonadIO m => BatchEnv -> FinishedSpan -> m ()
batchReporter BatchEnv{envQ} = liftIO . atomically . writeTQueue envQ

consumer
    :: Int
    -> TQueue FinishedSpan
    -> ([FinishedSpan] -> IO ())
    -> IO (Async ())
consumer siz q f = async . handle drain . forever $
    go (void . atomically $ peekTQueue q)
  where
    go onEmpty = do
        batch <- atomically $ pop siz q
        case batch of
            [] -> onEmpty
            xs -> mask_ $ f xs `catchAny` const (return ())

    drain ThreadKilled = go (return ())
    drain e            = throwM e

pop :: Int -> TQueue a -> STM [a]
pop 0 _ = pure []
pop n q = do
    v <- tryReadTQueue q
    case v of
        Nothing -> pure []
        Just v' -> (v' :) <$> pop (n-1) q
