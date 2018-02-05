{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module OpenTracing.Propagation
    ( TextMap
    , Headers
--  , Binary

    , Propagation
    , HasPropagation(..)

    , Carrier(..)
    , HasCarrier
    , HasCarriers
    , carrier

    , inject
    , extract

    , otPropagation
    , b3Propagation

    , _OTTextMap
    , _OTHeaders
    , _B3TextMap
    , _B3Headers

    , _Headers'

    -- * Re-exports from 'Data.Vinyl'
    , Rec ((:&), RNil)
    , rappend, (<+>)
    , rcast
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
import           Data.Proxy
import           Data.Text            (Text, isPrefixOf, toLower)
import           Data.Text.Encoding   (decodeUtf8, encodeUtf8)
import qualified Data.Text.Read       as Text
import           Data.Vinyl
import           Data.Word
import           Network.HTTP.Types   (Header)
import           OpenTracing.Span
import           OpenTracing.Types


type TextMap = HashMap Text Text
type Headers = [Header]
--type Binary  = Lazy.ByteString


type Propagation carriers = Rec Carrier carriers

class HasPropagation a p | a -> p where
    propagation :: Getting r a (Propagation p)

instance HasPropagation (Propagation p) p where
    propagation = id


newtype Carrier a = Carrier { fromCarrier :: Prism' a SpanContext }

type HasCarrier  c  cs = c  ∈ cs
type HasCarriers cs ds = cs ⊆ ds


carrier :: HasCarrier c cs => proxy c -> Propagation cs -> Prism' c SpanContext
carrier c = fromCarrier . view (rlens c)


inject :: forall c p. HasCarrier c p => Propagation p -> SpanContext -> c
inject p = review (carrier (Proxy @c) p)

extract :: forall c p. HasCarrier c p => Propagation p -> c -> Maybe SpanContext
extract p = preview (carrier (Proxy @c) p)


otPropagation :: Propagation '[TextMap, Headers]
otPropagation = Carrier _OTTextMap :& Carrier _OTHeaders :& RNil

b3Propagation :: Propagation '[TextMap, Headers]
b3Propagation = Carrier _B3TextMap :& Carrier _B3Headers :& RNil


_OTTextMap :: Prism' TextMap SpanContext
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


_OTHeaders :: Prism' Headers SpanContext
_OTHeaders = _Headers' _OTTextMap

_OTSampled :: Prism' Text Sampled
_OTSampled = prism' enc dec
    where
      enc = \case Sampled -> "1"
                  _       -> "0"

      dec = either (const Nothing) id
          . fmap (\(x,_) -> Just $ if x == (1 :: Word8) then Sampled else NotSampled)
          . Text.decimal

_B3TextMap :: Prism' TextMap SpanContext
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

_B3Headers :: Prism' Headers SpanContext
_B3Headers = _Headers' _B3TextMap

-- XXX: ensure headers are actually compliant with RFC 7230, Section 3.2.4
_Headers' :: Prism' TextMap SpanContext -> Prism' Headers SpanContext
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
