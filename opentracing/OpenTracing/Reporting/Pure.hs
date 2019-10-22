{-|
Module: OpenTracing.Reporting.Pure

Reporters with no external components.
-}

{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData      #-}

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

-- | A null reporter which ignores anything it's given.
noReporter :: MonadIO m => FinishedSpan -> m ()
noReporter = const $ pure ()

-- | A reporter which stores the finished spans in memory where
-- they wait to be consumed.
memReporter :: MonadIO m => Mem -> FinishedSpan -> m ()
memReporter m = liftIO . memAppend m

-- | Mem reporter state.
data Mem = Mem
    { siz :: Maybe Word32
    , vec :: IORef [FinishedSpan]
    }

-- | Construct a new `memReporter` environment that can store an unbounded
-- seequence of `FinishedSpan`s.
newMem :: IO Mem
newMem = Mem Nothing <$> newIORef []

-- | Construct a new `memReprioerter` environment that stores a bounded
-- sequence of `FinishedSpan`s
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

-- | View the `FinishedSpans` in a `memReporter` without removing them.
memPeek :: Mem -> IO [FinishedSpan]
memPeek Mem{vec} = readIORef vec

-- | View and remove the `FinishedSpans` in a `memReporter`.
memTake :: Mem -> IO [FinishedSpan]
memTake Mem{vec} = atomicModifyIORef' vec $ (,) []
