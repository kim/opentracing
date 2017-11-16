{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent     (threadDelay)
import Control.Monad.IO.Class (liftIO)
import OpenTracing
import OpenTracing.Standard   (newEnv, stdReporter, stdTracer)


main :: IO ()
main = do
    env <- newEnv (constSampler True)
    runTracing (Tracing (stdTracer env) stdReporter otPropagation) $
        traced__ (spanOpts "hello" mempty          ) $ \parent ->
        traced__ (spanOpts "world" (childOf parent)) $ \_child ->
            liftIO $ do
                putStrLn "doing some work..."
                threadDelay 500000
