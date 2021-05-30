{-|
Module: OpenTracing.Propagation

Types and functions for serializing and deserializing `SpanContext`s across
process boundaries.

One of the big motiviating use cases for propagation is for tracing distributed
executions through RPC calls.
-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}

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

    , _HeadersTextMap

    -- * Re-exports from 'Data.Vinyl'
    , Rec ((:&), RNil)
    , rappend, (<+>)
    , rcast
    )
where

import           Control.Applicative     ((<|>))
import           Control.Lens
import           Data.Bool               (bool)
import           Data.ByteString.Builder (toLazyByteString)
import qualified Data.CaseInsensitive    as CI
import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as HashMap
import           Data.Maybe              (catMaybes)
import           Data.Proxy
import           Data.Text               (Text, isPrefixOf, toLower)
import           Data.Text.Encoding      (decodeUtf8, encodeUtf8)
import qualified Data.Text.Read          as Text
import           Data.Vinyl
import           Data.Word
import           Network.HTTP.Types      (Header)
import           OpenTracing.Span
import           OpenTracing.Types
import           URI.ByteString          (urlDecodeQuery, urlEncodeQuery)


type TextMap = HashMap Text Text
type Headers = [Header]
--type Binary  = Lazy.ByteString

-- | A `Propagation` contains the different ways that a `SpanContext` can be
-- serialized and deserialized. For example @Propagation '[TextMap, Headers]@ indicates
-- support for serializing to `Header` or to `TextMap`.
--
-- @since 0.1.0.0
type Propagation carriers = Rec Carrier carriers

-- | A typeclass for application environments that contain a `Propagation`.
--
-- @since 0.1.0.0
class HasPropagation a p | a -> p where
    propagation :: Getting r a (Propagation p)

instance HasPropagation (Propagation p) p where
    propagation = id

-- | `Carrier a` is a way to convert a `SpanContext` into or from an `a`.
--
-- @since 0.1.0.0
newtype Carrier a = Carrier { fromCarrier :: Prism' a SpanContext }

type HasCarrier  c  cs = c  ∈ cs
type HasCarriers cs ds = cs ⊆ ds

-- | Retrieve a (de)serialization lens from the application context for
-- format @c@.
--
-- @since 0.1.0.0
carrier
    :: ( HasCarrier     c cs
       , HasPropagation r cs
       )
    => proxy c -- ^ Proxy for the carrier type @c@.
    -> r -- ^ The application context
    -> Prism' c SpanContext
carrier _c = fromCarrier . view (propagation . rlens)

-- | Serialize a `SpanContext` into the format `c` using a serializer from
-- the application context.
--
-- @since 0.1.0.0
inject
    :: forall c r p.
       ( HasCarrier     c p
       , HasPropagation r p
       )
    => r
    -> SpanContext
    -> c
inject r = review (carrier (Proxy @c) r)

-- | Attempt to deserialize a `SpanContext` from the format @c@ using a deserializer
-- from the application context
--
-- @since 0.1.0.0
extract
    :: forall c r p.
       ( HasCarrier     c p
       , HasPropagation r p
       )
    => r
    -> c
    -> Maybe SpanContext
extract r = preview (carrier (Proxy @c) r)


-- | A propagation using an "ot" prefix.
-- No parent span id is propagated in OT.
otPropagation :: Propagation '[TextMap, Headers]
otPropagation = Carrier _OTTextMap :& Carrier _OTHeaders :& RNil

-- | A propagation using an "x-b3" prefix for use with Zipkin.
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
_OTHeaders = _HeadersTextMap . _OTTextMap

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
_B3Headers = _HeadersTextMap . _B3TextMap

-- | Convert between a 'TextMap' and 'Headers'
--
-- Header field values are URL-encoded when converting from 'TextMap' to
-- 'Headers', and URL-decoded when converting the other way.
--
-- Note: validity of header fields is not checked (RFC 7230, 3.2.4)
_HeadersTextMap :: Iso' Headers TextMap
_HeadersTextMap = iso toTextMap toHeaders
  where
    toHeaders
        = map (bimap (CI.mk . encodeUtf8)
                     (view strict . toLazyByteString . urlEncodeQuery . encodeUtf8))
        . HashMap.toList

    toTextMap
        = HashMap.fromList
        . map (bimap (toLower . decodeUtf8 . CI.original)
                     (decodeUtf8 . urlDecodeQuery))
