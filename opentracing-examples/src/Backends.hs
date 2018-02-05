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

import           Control.Lens
import           Data.Semigroup         ((<>))
import           Data.Text              (Text)
import           Network.HTTP.Client    (defaultManagerSettings, newManager)
import           OpenTracing
import           OpenTracing.CloudTrace
import           OpenTracing.Jaeger
import           OpenTracing.Standard
import qualified OpenTracing.Zipkin.V1  as ZipkinV1
import           OpenTracing.Zipkin.V2  (Endpoint (..))
import qualified OpenTracing.Zipkin.V2  as ZipkinV2
import           Options.Applicative


data ZipkinAPI = V1 | V2
    deriving (Eq, Show, Read)

data Backend
    = Std
    | Zipkin ZipkinAPI
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
instance HasServiceName ZipkinV1.ZipkinOptions where
    srvName = ZipkinV1.zoLocalEndpoint
            . lens serviceName (\s a -> s { serviceName = a })
instance HasServiceName ZipkinV2.ZipkinOptions where
    srvName = ZipkinV2.zoLocalEndpoint
            . lens serviceName (\s a -> s { serviceName = a })


withBackend
    :: Backend
    -> (forall cfg. HasServiceName cfg => cfg -> cfg)
    -> (Tracer -> IO a)
    -> IO a
withBackend be cfg f = do
    std <- do
        env <- newStdEnv (constSampler True)
        return $ Tracer (stdTracer env) stdReporter

    case be of
        Std -> f std

        JaegerAgent -> do
            let opts = jaegerAgentOptions "jaeger-agent-example"
            withJaegerAgent (cfg opts) $ \j ->
                f std { tracerReport = jaegerAgentReporter j }

        JaegerCollector -> do
            mgr <- newManager defaultManagerSettings
            let opts = jaegerCollectorOptions mgr "jaeger-collector-example"
            withJaegerCollector (cfg opts) $ \j ->
                f std { tracerReport = jaegerCollectorReporter j }

        Zipkin V1 -> do
            mgr <- newManager defaultManagerSettings
            let opts = ZipkinV1.zipkinOptions mgr Endpoint
                     { serviceName = "zipkin-example"
                     , ipv4        = read "127.0.0.1"
                     , ipv6        = Nothing
                     , port        = Nothing
                     }
            ZipkinV1.withZipkin (cfg opts) $ \z ->
                f std { tracerReport = ZipkinV1.zipkinHttpReporter z }

        Zipkin V2 -> do
            mgr <- newManager defaultManagerSettings
            let opts = ZipkinV2.zipkinOptions mgr Endpoint
                     { serviceName = "zipkin-example"
                     , ipv4        = read "127.0.0.1"
                     , ipv6        = Nothing
                     , port        = Nothing
                     }
            ZipkinV2.withZipkin (cfg opts) $ \z ->
                f std { tracerReport = ZipkinV2.zipkinHttpReporter z }

        CloudTrace pid -> do
            opt <- simpleCloudTraceOptions pid Nothing
            withCloudTrace opt $ \c ->
                f std { tracerReport = cloudTraceReporter c }
