{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent         (threadDelay)
import Control.Lens               (view)
import Control.Monad.IO.Class
import OpenTracing
import OpenTracing.Simple


main :: IO ()
main = do
    env <- newEnv (constSampler True)
    runTracing (Tracing (simpleTracer env) simpleReporter) $
        traced (spanOpts "hello" mempty          ) $ \parent ->
        traced (spanOpts "world" [childOf parent]) $ \_child ->
            liftIO $ do
                putStrLn "doing some work..."
                threadDelay 500000

childOf :: Span ctx -> Reference ctx
childOf = ChildOf . view spanContext
