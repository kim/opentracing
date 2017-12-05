{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}

module OpenTracing.Propagation
    ( Propagation(..)
    , HasPropagation(..)
    , OpenTracing
    , B3

    , otPropagation
    , b3Propagation

    , _OTTextMap
    , _OTHeaders
    , _B3TextMap
    , _B3Headers

    , _Headers'
    )
where

import           Control.Applicative  ((<|>))
import           Control.Lens
import           Data.Bool            (bool)
import qualified Data.CaseInsensitive as CI
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as HashMap
import           Data.Maybe           (catMaybes)
import           Data.Monoid
import           Data.Text            (Text, isPrefixOf, toLower)
import           Data.Text.Encoding   (decodeUtf8, encodeUtf8)
import qualified Data.Text.Read       as Text
import           Data.Word
import           Network.HTTP.Types   (Header)
import           OpenTracing.Span
import           OpenTracing.Types


data Propagation (a :: k) = Propagation

class HasPropagation a c where
    propagation :: proxy a -> Prism' c SpanContext

data OpenTracing
data B3


otPropagation :: Propagation OpenTracing
otPropagation = Propagation

instance HasPropagation (Propagation OpenTracing) (HashMap Text Text) where
    propagation _ = _OTTextMap

instance HasPropagation (Propagation OpenTracing) [Header] where
    propagation _ = _OTHeaders


b3Propagation :: Propagation B3
b3Propagation = Propagation

instance HasPropagation (Propagation B3) (HashMap Text Text) where
    propagation _ = _B3TextMap

instance HasPropagation (Propagation B3) [Header] where
    propagation _ = _B3Headers


_OTTextMap :: Prism' (HashMap Text Text) SpanContext
_OTTextMap = prism' fromCtx toCtx
  where
    fromCtx c@SpanContext{..} = HashMap.fromList $
          ("ot-tracer-traceid", view hexText ctxTraceID)
        : ("ot-tracer-spanid" , view hexText ctxSpanID)
        : ("ot-tracer-sampled", view (ctxSampled . re _OTSampled) c)
        : map (over _1 ("ot-baggage-" <>)) (HashMap.toList _ctxBaggage)

    toCtx m = SpanContext
        <$> (HashMap.lookup "ot-tracer-traceid" m >>= preview _Hex . knownHex)
        <*> (HashMap.lookup "ot-tracer-spanid"  m >>= preview _Hex . knownHex)
        <*> pure Nothing -- nb. parent is not propagated in OT
        <*> (HashMap.lookup "ot-tracer-sampled" m >>= preview _OTSampled)
        <*> pure (HashMap.filterWithKey (\k _ -> "ot-baggage-" `isPrefixOf` k) m)


_OTHeaders :: Prism' [Header] SpanContext
_OTHeaders = _Headers' _OTTextMap

_OTSampled :: Prism' Text Sampled
_OTSampled = prism' enc dec
    where
      enc = \case Sampled -> "1"
                  _       -> "0"

      dec = either (const Nothing) id
          . fmap (\(x,_) -> Just $ if x == (1 :: Word8) then Sampled else NotSampled)
          . Text.decimal

_B3TextMap :: Prism' (HashMap Text Text) SpanContext
_B3TextMap = prism' fromCtx toCtx
  where
    fromCtx ctx@SpanContext{..} = HashMap.fromList . catMaybes $
          Just ("x-b3-traceid", view hexText ctxTraceID)
        : Just ("x-b3-spanid" , view hexText ctxSpanID)
        : fmap (("x-b3-parentspanid",) . view hexText) ctxParentSpanID
        : Just ("x-b3-sampled", bool "false" "true" $ view (ctxSampled . re _IsSampled) ctx)
        : map (Just . over _1 ("ot-baggage-" <>)) (HashMap.toList _ctxBaggage)

    toCtx m = SpanContext
        <$> (HashMap.lookup "x-b3-traceid" m >>= preview _Hex . knownHex)
        <*> (HashMap.lookup "x-b3-spanid"  m >>= preview _Hex . knownHex)
        <*> (Just $ HashMap.lookup "x-b3-parentspanid" m >>= preview _Hex . knownHex)
        <*> (b3Sampled m <|> b3Debug m <|> Just NotSampled)
        <*> pure (HashMap.filterWithKey (\k _ -> "ot-baggage-" `isPrefixOf` k) m)

    b3Sampled m = HashMap.lookup "x-b3-sampled" m >>= \case
        "true" -> Just Sampled
        _      -> Nothing

    b3Debug m = HashMap.lookup "x-b3-flags" m >>= \case
        "1" -> Just Sampled
        _   -> Nothing

_B3Headers :: Prism' [Header] SpanContext
_B3Headers = _Headers' _B3TextMap

-- XXX: ensure headers are actually compliant with RFC 7230, Section 3.2.4
_Headers' :: Prism' (HashMap Text Text) SpanContext -> Prism' [Header] SpanContext
_Headers' _TextMap = prism' fromCtx toCtx
  where
    fromCtx
        = map (bimap (CI.mk . encodeUtf8) encodeUtf8)
        . HashMap.toList
        . review _TextMap

    toCtx
        = preview _TextMap
        . HashMap.fromList
        . map (bimap (toLower . decodeUtf8 . CI.original) decodeUtf8)
