{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StrictData                 #-}

module OpenTracing.Types
    ( TraceID(..)
    , IPv4(..)
    , IPv6(..)
    , Port(..)

    , Protocol(..)
    , Addr(..)
    , addrHostName
    , addrPort
    , addrSecure

    , Hex
    , knownHex

    , AsHex(..)
    , hexText
    )
where

import           Control.Lens
import           Data.Aeson                 (ToJSON (..))
import           Data.Aeson.Encoding
import qualified Data.IP                    as IP
import           Data.Monoid                (Monoid)
import           Data.Semigroup             (Semigroup, (<>))
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Lazy.Builder     as TB
import qualified Data.Text.Lazy.Builder.Int as TB
import qualified Data.Text.Read             as TR
import           Data.Word
import           Network                    (HostName)


data TraceID = TraceID
    { traceIdHi :: Maybe Word64
    , traceIdLo :: Word64
    } deriving (Eq, Ord, Show)


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


data Protocol = UDP | HTTP

data Addr a where
    UDPAddr  :: HostName -> Port         -> Addr 'UDP
    HTTPAddr :: HostName -> Port -> Bool -> Addr 'HTTP

addrHostName :: Lens' (Addr a) HostName
addrHostName f (UDPAddr  h p  ) = (\h' -> UDPAddr  h' p  ) <$> f h
addrHostName f (HTTPAddr h p s) = (\h' -> HTTPAddr h' p s) <$> f h

addrPort :: Lens' (Addr a) Port
addrPort f (UDPAddr  h p  ) = (\p' -> UDPAddr  h p'  ) <$> f p
addrPort f (HTTPAddr h p s) = (\p' -> HTTPAddr h p' s) <$> f p

addrSecure :: Lens' (Addr 'HTTP) Bool
addrSecure f (HTTPAddr h p s) = (\s' -> HTTPAddr h p s') <$> f s


newtype Hex = Hex { unHex :: Text }
    deriving (Eq, Show, Monoid, Semigroup)

knownHex :: Text -> Hex
knownHex = Hex

class AsHex a where
    _Hex :: Prism' Hex a

instance AsHex TraceID where
    _Hex = prism' enc dec
      where
        enc (TraceID hi lo)
            = Hex . unHex $ maybe mempty (review _Hex) hi <> review _Hex lo

        dec (Hex t)
            = case Text.splitAt 16 t of
                  ("", lo) -> TraceID Nothing <$> preview _Hex (Hex lo)
                  (hi, lo) -> TraceID <$> Just (preview _Hex (Hex hi))
                                      <*> preview _Hex (Hex lo)
    {-# INLINE _Hex #-}

instance AsHex Word64 where
    _Hex = prism' enc dec
      where
        enc = Hex . view strict . TB.toLazyText . TB.hexadecimal
        dec = either (const Nothing) (pure . fst) . TR.hexadecimal . unHex
    {-# INLINE _Hex #-}

hexText :: AsHex a => Getter a Text
hexText = re _Hex . to unHex
{-# INLINE hexText #-}
