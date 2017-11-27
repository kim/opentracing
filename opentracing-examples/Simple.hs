{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent                   (threadDelay)
import Control.Monad.IO.Class               (liftIO)
import Network.HTTP.Client                  (defaultManagerSettings, newManager)
import OpenTracing
import OpenTracing.Jaeger
import OpenTracing.Standard
import OpenTracing.Zipkin
import System.Environment                   (getArgs)


data Backend = Std | Zipkin1 | Zipkin2 | JaegerAgent | JaegerCollector
    deriving (Show, Read)

main :: IO ()
main = do
    be <- read . head <$> getArgs
    setup be $ \tr -> runTracing tr $
        traced__ (spanOpts "hello" mempty          ) $ \parent ->
        traced__ (spanOpts "world" (childOf parent)) $ \_child ->
            liftIO $ do
                putStrLn "doing some work..."
                threadDelay 500000

setup :: Backend -> (Tracing -> IO a) -> IO a
setup be f = do
    std <- do
        env <- newStdEnv (constSampler True)
        return $ Tracing (stdTracer env) stdReporter otPropagation

    case be of
        Std -> f std

        JaegerAgent -> do
            let opts = jaegerAgentOptions "jaeger-agent-example"
            withJaegerAgent opts $ \j ->
                f std { traceReport        = jaegerAgentReporter j
                      , tracingPropagation = jaegerPropagation
                      }

        JaegerCollector -> do
            mgr <- newManager defaultManagerSettings
            let opts = jaegerCollectorOptions mgr "jaeger-collector-example"
            withJaegerCollector opts $ \j ->
                f std { traceReport        = jaegerCollectorReporter j
                      , tracingPropagation = jaegerPropagation
                      }

        Zipkin1 -> zipkin V1 std
        Zipkin2 -> zipkin V2 std
  where
    zipkin api std = do
        mgr <- newManager defaultManagerSettings
        let opts = zipkinOptions mgr api Endpoint
                 { serviceName = "zipkin-example"
                 , ipv4        = read "127.0.0.1"
                 , ipv6        = Nothing
                 , port        = Nothing
                 }
        withZipkin opts $ \z ->
            f std { traceReport        = zipkinHttpReporter z
                  , tracingPropagation = b3Propagation
                  }
