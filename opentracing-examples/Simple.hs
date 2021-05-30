{-# LANGUAGE OverloadedStrings #-}

module Main where

import Backends
import Control.Concurrent     (threadDelay)
import Control.Monad.IO.Class (liftIO)
import OpenTracing
import Options.Applicative


options :: ParserInfo Backend
options = info (parseBackend <**> helper)
    ( fullDesc
   <> progDesc "Simplistic tracing demo"
    )

main :: IO ()
main = do
    be <- execParser options
    withBackend be id $ flip runTracer $
        traced_ (spanOpts "hello" mempty          ) $ \parent ->
        traced_ (spanOpts "world" (childOf parent)) $ \child ->
            liftIO $ do
                putStrLn "doing some work..."
                addLogRecord child (Message "doing some work")
                threadDelay 500000
