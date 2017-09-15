{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent     (threadDelay)
import Control.Lens           (view)
import Control.Monad.IO.Class
import OpenTracing


main :: IO ()
main = do
    env <- newEnv FromEnv
    runTracer env $
        traceStartSafe "hello" mempty           mempty $ \parent ->
        traceStartSafe "world" [childOf parent] mempty $ \_child ->
            liftIO $ do
                putStrLn "doing some work..."
                threadDelay 500000

childOf :: Span Context -> Reference Context
childOf = ChildOf . view spanContext
