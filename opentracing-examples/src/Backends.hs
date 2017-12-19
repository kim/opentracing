{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE StrictData        #-}

module Backends
    ( Backend(..)
    , parseBackend
    , withBackend
    , srvName
    )
where

import Control.Lens
import Data.Semigroup         ((<>))
import Data.Text              (Text)
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


parseBackend :: Parser Backend
parseBackend = std <|> zipkin <|> jaegerAgent <|> jaegerCollector <|> cloudtrace
  where
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


class HasServiceName a where
    srvName :: Lens' a Text

instance HasServiceName JaegerAgentOptions     where srvName = jaoServiceName
instance HasServiceName JaegerCollectorOptions where srvName = jcoServiceName
instance HasServiceName ZipkinOptions          where
    srvName = zoLocalEndpoint . lens serviceName (\s a -> s { serviceName = a })


withBackend
    :: Backend
    -> (forall cfg. HasServiceName cfg => cfg -> cfg)
    -> (Tracing -> IO a)
    -> IO a
withBackend be cfg f = do
    std <- do
        env <- newStdEnv (constSampler True)
        return $ Tracing (stdTracer env) stdReporter

    case be of
        Std -> f std

        JaegerAgent -> do
            let opts = jaegerAgentOptions "jaeger-agent-example"
            withJaegerAgent (cfg opts) $ \j ->
                f std { traceReport = jaegerAgentReporter j }

        JaegerCollector -> do
            mgr <- newManager defaultManagerSettings
            let opts = jaegerCollectorOptions mgr "jaeger-collector-example"
            withJaegerCollector (cfg opts) $ \j ->
                f std { traceReport = jaegerCollectorReporter j }

        Zipkin api -> do
            mgr <- newManager defaultManagerSettings
            let opts = zipkinOptions mgr api Endpoint
                     { serviceName = "zipkin-example"
                     , ipv4        = read "127.0.0.1"
                     , ipv6        = Nothing
                     , port        = Nothing
                     }
            withZipkin (cfg opts) $ \z ->
                f std { traceReport = zipkinHttpReporter z }

        CloudTrace pid -> do
            opt <- simpleCloudTraceOptions pid Nothing
            withCloudTrace opt $ \c ->
                f std { traceReport = cloudTraceReporter c }
