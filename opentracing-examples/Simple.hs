{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent                   (threadDelay)
import           Control.Monad.IO.Class               (liftIO)
import           Network.HTTP.Client
    ( defaultManagerSettings
    , newManager
    )
import           OpenTracing
import           OpenTracing.Jaeger.AgentReporter
    ( jaegerAgentOptions
    , jaegerAgentReporter
    )
import qualified OpenTracing.Jaeger.AgentReporter     as JA
import           OpenTracing.Jaeger.CollectorReporter
    ( jaegerCollectorOptions
    , jaegerCollectorReporter
    )
import qualified OpenTracing.Jaeger.CollectorReporter as JC
import           OpenTracing.Jaeger.Propagation       (jaegerPropagation)
import           OpenTracing.Standard                 (stdReporter, stdTracer)
import qualified OpenTracing.Standard                 as Std
import           OpenTracing.Zipkin.HttpReporter
    ( zipkinHttpReporter
    , zipkinHttpOptions
    )
import qualified OpenTracing.Zipkin.HttpReporter      as Z
import           System.Environment                   (getArgs)


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
        env <- Std.newEnv (constSampler True)
        return $ Tracing (stdTracer env) stdReporter otPropagation

    case be of
        Std -> f std

        JaegerAgent -> do
            let opts = jaegerAgentOptions "jaeger-agent-example"
            JA.withEnv opts $ \jenv ->
                f std { traceReport        = jaegerAgentReporter jenv
                      , tracingPropagation = jaegerPropagation
                      }

        JaegerCollector -> do
            mgr <- newManager defaultManagerSettings
            let opts = jaegerCollectorOptions mgr "jaeger-collector-example"
            JC.withEnv opts $ \jenv ->
                f std { traceReport        = jaegerCollectorReporter jenv
                      , tracingPropagation = jaegerPropagation
                      }
        zipkin -> do
            mgr <- newManager defaultManagerSettings
            let apiv = case zipkin of { Zipkin1 -> Z.V1; _ -> Z.V2 }
            let opts = zipkinHttpOptions mgr apiv Z.Endpoint
                     { Z.serviceName = "zipkin-example"
                     , Z.ipv4        = read "127.0.0.1"
                     , Z.ipv6        = Nothing
                     , Z.port        = Nothing
                     }
            Z.withEnv opts $ \zenv ->
                f std { traceReport        = zipkinHttpReporter zenv
                      , tracingPropagation = b3Propagation
                      }

