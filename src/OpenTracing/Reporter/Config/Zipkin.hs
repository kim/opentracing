{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Zipkin.Reporter.Config where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Encoding
import Data.IP                (IPv4, IPv6)
import Data.Maybe
import Data.Maybe             (catMaybes)
import Data.Text              (Text, pack)
import GHC.Generics           (Generic)
import Network                (PortNumber)
import Network.HTTP.Client
import OpenTracing.Types
import System.Environment
import Text.Read              (readMaybe)


data Config = Config
    { cfgReq           :: Request
    , cfgMgr           :: Manager
    , cfgLocalEndpoint :: Endpoint
    }

data Endpoint = Endpoint
    { serviceName :: Maybe Text
    , ipv4        :: Maybe IPv4
    , ipv6        :: Maybe IPv6
    , port        :: Maybe PortNumber
    } deriving (Eq, Show, Generic)

instance ToJSON Endpoint where
    toJSON Endpoint{..} = object . catMaybes $
        [ ("serviceName" .=)     <$> serviceName
        , ("ipv4" .=) . show     <$> ipv4
        , ("ipv6" .=) . show     <$> ipv6
        , ("port" .=) . fromEnum <$> port
        ]

    toEncoding Endpoint{..} = pairs . mconcat . catMaybes $
        [ pair "serviceName" . text           <$> serviceName
        , pair "ipv4" . string . show         <$> ipv4
        , pair "ipv6" . string . show         <$> ipv6
        , pair "port" . word16 . fromIntegral <$> port
        ]


loadConfig :: MonadIO m => ConfigSource -> m Config
loadConfig FromEnv = liftIO $ do
    le <- Endpoint
      <$> (Just . pack <$> getEnv "SERVICE_NAME")
      <*> (readMaybe <$> getEnv "HOST")
      <*> (readMaybe <$> getEnv "HOST")
      <*> (readMaybe <$> getEnv "PORT")
    zh <- getEnv "ZIPKIN_HOST"
    zp <- fromMaybe "9411" <$> lookupEnv "ZIPKIN_PORT"
    Config <$> parseRequest ("POST " ++ zh ++ ":" ++ zp ++ "/api/v2")
           <*> newManager defaultManagerSettings
           <*> pure le
