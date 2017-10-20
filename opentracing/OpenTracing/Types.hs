{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module OpenTracing.Types
    ( HasTraceID(..)
    , IPv4(..)
    , IPv6(..)
    , Port(..)
    )
where

import           Control.Lens
import           Data.Aeson          (ToJSON (..))
import           Data.Aeson.Encoding
import qualified Data.IP             as IP
import           Data.Word


class HasTraceID a where
    traceIdHi :: a -> Maybe Word64
    traceIdLo :: a -> Word64

instance HasTraceID Word64 where
    traceIdHi = const Nothing
    traceIdLo = id
    {-# INLINE traceIdHi #-}
    {-# INLINE traceIdLo #-}


newtype IPv4 = IPv4 { fromIPv4 :: IP.IPv4 }
    deriving (Bounded, Enum, Eq, Ord)

newtype IPv6 = IPv6 { fromIPv6 :: IP.IPv6 }
    deriving (Bounded, Enum, Eq, Ord)

instance Show IPv4 where show = show . fromIPv4
instance Show IPv6 where show = show . fromIPv6

instance Read IPv4 where readsPrec p = map (over _1 IPv4) . readsPrec p
instance Read IPv6 where readsPrec p = map (over _1 IPv6) . readsPrec p

instance ToJSON IPv4 where
    toJSON     = toJSON . show . fromIPv4
    toEncoding = string . show . fromIPv4

instance ToJSON IPv6 where
    toJSON     = toJSON . show . fromIPv6
    toEncoding = string . show . fromIPv6


newtype Port = Port { fromPort :: Word16 }
    deriving (Enum, Eq, Num, Ord)

instance Show Port where show = show . fromPort
instance Read Port where readsPrec p = map (over _1 Port) . readsPrec p

instance ToJSON Port where
    toJSON     = toJSON . fromPort
    toEncoding = word16 . fromPort
