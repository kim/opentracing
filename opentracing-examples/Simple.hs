{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent     (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Data.Semigroup         ((<>))
import OpenTracing
import OpenTracing.CloudTrace
import OpenTracing.Jaeger
import OpenTracing.Standard
import OpenTracing.Zipkin
import Options.Applicative


data Backend
    = Std
    | Zipkin API
    | JaegerAgent
    | JaegerCollector
    | CloudTrace ProjectId
    deriving (Show, Read)

options :: ParserInfo Backend
options = info (parser <**> helper)
    ( fullDesc
   <> progDesc "Simplistic tracing demo"
    )
  where
    parser = std <|> zipkin <|> jaegerAgent <|> jaegerCollector <|> cloudtrace

    std = flag' Std
        ( long "std"
       <> help "Standard tracer reporting to stdout"
        )

    zipkin = flag' ()
        ( long "zipkin"
       <> help "Report to a Zipkin server running on localhost"
        )

        *> ( Zipkin
          <$> option auto
              ( short 'v'
             <> long "version"
             <> help "Zipkin API version"
             <> value V2
             <> showDefault
              )
           )

    jaegerAgent = flag' JaegerAgent
        ( long "jaeger-agent"
       <> help "Report via UDP to a Jaeger Agent running on localhost"
        )

    jaegerCollector = flag' JaegerCollector
        ( long "jaeger-collector"
       <> help "Report via HTTP to a Jaeger Collector running on localhost"
        )

    cloudtrace = flag' ()
        ( long "cloudtrace"
       <> help "Report to Google CloudTrace (aka Stackdriver Trace)"
        )

        *> ( CloudTrace
          <$> strOption
              ( short 'p'
             <> long "project"
             <> help "GCloud project id"
              )
           )

main :: IO ()
main = do
    be <- execParser options
    setup be $ flip runTracing $
        traced__ (spanOpts "hello" mempty          ) $ \parent ->
        traced__ (spanOpts "world" (childOf parent)) $ \child ->
            liftIO $ do
                putStrLn "doing some work..."
                addLogRecord child (Message "doing some work")
                threadDelay 500000

setup :: Backend -> (Tracing -> IO a) -> IO a
setup be f = do
    std <- do
        env <- newStdEnv (constSampler True)
        return $ Tracing (stdTracer env) stdReporter

    case be of
        Std -> f std

        JaegerAgent -> do
            let opts = jaegerAgentOptions "jaeger-agent-example"
            withJaegerAgent opts $ \j ->
                f std { traceReport = jaegerAgentReporter j }

        JaegerCollector -> do
            mgr <- newManager defaultManagerSettings
            let opts = jaegerCollectorOptions mgr "jaeger-collector-example"
            withJaegerCollector opts $ \j ->
                f std { traceReport = jaegerCollectorReporter j }

        Zipkin api -> do
            mgr <- newManager defaultManagerSettings
            let opts = zipkinOptions mgr api Endpoint
                     { serviceName = "zipkin-example"
                     , ipv4        = read "127.0.0.1"
                     , ipv6        = Nothing
                     , port        = Nothing
                     }
            withZipkin opts $ \z ->
                f std { traceReport = zipkinHttpReporter z }

        CloudTrace pid -> do
            opt <- simpleCloudTraceOptions pid Nothing
            withCloudTrace opt $ \c ->
                f std { traceReport = cloudTraceReporter c }
