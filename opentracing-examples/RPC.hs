{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Main where

import           Backends
import           Control.Concurrent                 (threadDelay)
import           Control.Concurrent.Async
import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.Fix
import           Control.Monad.Managed
import           Control.Monad.Reader
import qualified Data.ByteString                    as BS
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy.Char8         as BLC
import           Data.Foldable
import           Data.IORef
import           Data.Semigroup
import           Network.HTTP.Client
import           Network.HTTP.Client.OpenTracing
import           Network.HTTP.Types
import           Network.Wai                        hiding
    ( Request
    , Response
    , requestBody
    )
import qualified Network.Wai                        as Wai
import qualified Network.Wai.Handler.Warp           as Warp
import           Network.Wai.Middleware.OpenTracing
import           OpenTracing
import           Options.Applicative
import           Prelude                            hiding (span)


data Env = Env
    { envTracing     :: Tracing
    , envPropagation :: Propagation '[Headers]
    , envManager     :: Manager
    }

instance HasTracing     Env            where tracing     = to envTracing
instance HasPropagation Env '[Headers] where propagation = to envPropagation


options :: ParserInfo Backend
options = info (parseBackend <**> helper)
    ( fullDesc
   <> progDesc "RPC tracing demo"
    )

main :: IO ()
main = do
    be  <- execParser options
    mgr <- newManager defaultManagerSettings
    let partialEnv = mkEnv be mgr
    race_ (runServers partialEnv) (runClient partialEnv)
  where
    runClient penv = runManaged $ do
        env <- penv "rpc-example-client"
        liftIO $ do
            res <- runReaderT client env
            for_ res $
                print . tracedResult

    servers =
        [ ("rpc-example-echo-server" , 1234, echoServer)
        , ("rpc-example-error-server", 1235, const . const errorServer)
        , ("rpc-example-event-server", 1236, const . const eventServer)
        ]

    runServers penv = forConcurrently_ servers $ \(s,p,m) -> runManaged $ do
        env <- penv s
        liftIO $ warpTraced env p (m env)

    warpTraced Env{..} port =
        Warp.run port . opentracing envTracing envPropagation

    mkTracing be srv = managed $ withBackend be (set srvName srv)
    mkEnv be mgr srv = do
        tr <- mkTracing be srv
        pure Env
            { envTracing     = tr
            , envPropagation = rcast otPropagation
            , envManager     = mgr
            }


client
    :: ( MonadReader Env m
       , MonadIO         m
       , MonadMask       m
       )
    => m [Traced (Response BLC.ByteString)]
client = do
    mgr <- asks envManager

    rq1 <- set rqBody (RequestBodyLBS "abcdef")
       <$> parseRequest "POST http://localhost:1234/echo"
    rq2 <- parseRequest "POST http://localhost:1235/error"

    traced_ (spanOpts "client" mempty) $ \root -> do
        rpc1 <- httpTraced (childOf root) rq1 mgr httpLbs
        rpc2 <- httpTraced (childOf root) rq2 mgr httpLbs
        return [rpc1, rpc2]


echoServer :: Env -> TracedApplication
echoServer _ _ req respond | requestMethod req /= "POST" =
    respond methodNotAllowed

echoServer env@Env{envManager=mgr} span req respond = respond $
    responseStream status200 [] $ \write flush -> do
        cnt <- newIORef (0 :: Int)

        fix $ \loop -> do
            chunk <- Wai.requestBody req
            write (byteString chunk) *> flush
            modifyIORef' cnt (+ BS.length chunk)
            unless (BS.null chunk) loop

        msg <- (byteString "ECHOED:" <>) . intDec <$> readIORef cnt
        rq  <- set rqBody (RequestBodyLBS (toLazyByteString msg))
           <$> parseRequest "PUT http://localhost:1236/events"
        void . flip runReaderT env $
            httpTraced (childOf span) rq mgr httpLbs


errorServer :: Application
errorServer _ = ($ serverError)


eventServer :: Application
eventServer req respond | requestMethod req /= "PUT" = respond methodNotAllowed
eventServer req respond = do
    strictRequestBody req >>= BLC.putStrLn
    threadDelay 5000
    respond accepted


notFound,methodNotAllowed,serverError,accepted :: Wai.Response
notFound         = rempty status404
methodNotAllowed = rempty status405
serverError      = rempty status500
accepted         = rempty status202

rempty :: Status -> Wai.Response
rempty s = responseLBS s [] mempty

rqBody :: Lens' Request RequestBody
rqBody = lens requestBody (\s a -> s { requestBody = a })
