{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Main where

import Control.Concurrent     (threadDelay)
import Control.Monad          (void)
import Control.Monad.IO.Class
import OpenTracing
import OpenTracing.Standard


main :: IO ()
main = do
    env <- newEnv (constSampler True)
    void . runTracing (Tracing (stdTracer env) stdReporter) $
        traced (spanOpts "hello" mempty          ) $ \parent ->
        traced (spanOpts "world" (childOf parent)) $ \_child -> do
            liftIO $ do
                putStrLn "doing some work..."
                threadDelay 500000
