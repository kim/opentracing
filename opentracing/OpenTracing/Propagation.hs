{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module OpenTracing.Propagation
    ( TextMap(..)
    , textMap

    , HttpHeaders(..)
    , httpHeaders

    , AsCarrier(..)

    , traceInject
    , traceExtract
    )
where

import Control.Lens
import Data.HashMap.Strict (HashMap)
import Data.Text           (Text)
import Network.HTTP.Types  (Header)


newtype TextMap ctx = TextMap { fromTextMap :: HashMap Text Text }
    deriving (Eq, Show, Monoid)

newtype HttpHeaders ctx = HttpHeaders { fromHttpHeaders :: [Header] }
    deriving (Eq, Show, Monoid)

{-
   skip for now: not clear what this is for (the whole stack would need to
   agree on the encoding)
newtype Binary ctx = Binary { fromBinary :: Lazy.ByteString }
    deriving (Eq, Show, Monoid)

binary :: Binary ctx
binary = mempty
-}

textMap :: TextMap ctx
textMap = mempty

httpHeaders :: HttpHeaders ctx
httpHeaders = mempty


class AsCarrier a x ctx | a x -> ctx where
    _Carrier :: Prism' (a x) ctx

traceInject :: AsCarrier a ctx ctx => ctx -> a ctx
traceInject = review _Carrier

traceExtract :: AsCarrier a ctx ctx => a ctx -> Maybe ctx
traceExtract = preview _Carrier
