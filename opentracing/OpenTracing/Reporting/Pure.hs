{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module OpenTracing.Reporting.Pure
    ( noReporter
    , memReporter
    , newMem
    , newBoundedMem
    , memPeek
    , memTake
    )
where

import Control.Monad.IO.Class
import Data.IORef
import Data.Word
import OpenTracing.Span


noReporter :: MonadIO m => FinishedSpan -> m ()
noReporter = const $ pure ()

memReporter :: MonadIO m => Mem -> FinishedSpan -> m ()
memReporter m = liftIO . memAppend m

data Mem = Mem
    { siz :: Maybe Word32
    , vec :: IORef [FinishedSpan]
    }

newMem :: IO Mem
newMem = Mem Nothing <$> newIORef []

newBoundedMem :: Word32 -> IO Mem
newBoundedMem s = Mem (Just s) <$> newIORef []

memAppend :: Mem -> FinishedSpan -> IO ()
memAppend Mem{..} x = atomicModifyIORef' vec $ \xs ->
    let xs' = case siz of
                  Nothing -> x : xs
                  Just  0 -> []
                  Just  1 -> [x]
                  Just  s -> x : take (fromIntegral s - 1) xs
     in (xs', ())

memPeek :: Mem -> IO [FinishedSpan]
memPeek Mem{vec} = readIORef vec

memTake :: Mem -> IO [FinishedSpan]
memTake Mem{vec} = atomicModifyIORef' vec $ (,) []
