{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}

module OpenTracing.Zipkin.Types
    ( Endpoint (..)
    )
where

import Data.Aeson
import Data.Aeson.Encoding
import Data.Maybe          (catMaybes)
import Data.Text           (Text)
import GHC.Generics        (Generic)
import OpenTracing.Types


data Endpoint = Endpoint
    { serviceName :: Maybe Text
    , ipv4        :: Maybe IPv4
    , ipv6        :: Maybe IPv6
    , port        :: Maybe Port
    } deriving (Eq, Show, Generic)

instance ToJSON Endpoint where
    toEncoding Endpoint{..} = pairs . mconcat . catMaybes $
        [ pair "serviceName" . text <$> serviceName
        , pair "ipv4" . toEncoding  <$> ipv4
        , pair "ipv6" . toEncoding  <$> ipv6
        , pair "port" . toEncoding  <$> port
        ]
