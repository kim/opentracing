{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent         (threadDelay)
import Control.Lens               (view)
import Control.Monad.IO.Class
import OpenTracing
import OpenTracing.Simple


main :: IO ()
main = do
    env <- newEnv
    runTracing (Tracing (simpleTracer env) simpleReporter) $
        traced (SpanOpts "hello" mempty           mempty) $ \parent ->
        traced (SpanOpts "world" [childOf parent] mempty) $ \_child ->
            liftIO $ do
                putStrLn "doing some work..."
                threadDelay 500000

childOf :: Span ctx -> Reference ctx
childOf = ChildOf . view spanContext
