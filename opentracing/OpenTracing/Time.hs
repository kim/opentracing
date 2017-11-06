module OpenTracing.Time (AsMicros(micros)) where

import Data.Int
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Word


class AsMicros a where
    micros :: Integral b => a -> b

instance AsMicros UTCTime where
    micros = round . (1000000*) . utcTimeToPOSIXSeconds
    {-# INLINE micros #-}

instance AsMicros NominalDiffTime where
    micros = round . (1000000*)
    {-# INLINE micros #-}

{-# SPECIALIZE micros :: UTCTime         -> Int64  #-}
{-# SPECIALIZE micros :: UTCTime         -> Word64 #-}
{-# SPECIALIZE micros :: NominalDiffTime -> Int64  #-}
{-# SPECIALIZE micros :: NominalDiffTime -> Word64 #-}
