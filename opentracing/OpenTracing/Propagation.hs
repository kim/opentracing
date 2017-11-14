module OpenTracing.Propagation (Propagation(..)) where

import           Control.Lens
import qualified Data.CaseInsensitive as CI
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as HashMap
import           Data.Text            (Text, toLower)
import           Data.Text.Encoding   (decodeUtf8, encodeUtf8)
import           Network.HTTP.Types   (Header)

class Propagation ctx where
    _TextMap :: Prism' (HashMap Text Text) ctx
    _Headers :: Prism' [Header]            ctx

    -- XXX: ensure headers are actually compliant with RFC 7230, Section 3.2.4
    _Headers = prism' fromCtx toCtx
      where
        fromCtx
            = map (bimap (CI.mk . encodeUtf8) encodeUtf8)
            . HashMap.toList
            . review _TextMap

        toCtx
            = preview _TextMap
            . HashMap.fromList
            . map (bimap (toLower . decodeUtf8 . CI.original) decodeUtf8)
