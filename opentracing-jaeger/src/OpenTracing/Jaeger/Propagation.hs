{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module OpenTracing.Jaeger.Propagation
    ( jaegerPropagation

    , _JaegerTextMap
    , _JaegerHeaders

    , _UberTraceId
    )
where

import           Control.Lens
import           Data.Bits
import qualified Data.HashMap.Strict     as HashMap
import           Data.Monoid
import           Data.Text               (Text, isPrefixOf)
import qualified Data.Text               as Text
import qualified Data.Text.Read          as Text
import           OpenTracing.Propagation
import           OpenTracing.Span
import           OpenTracing.Types


jaegerPropagation :: Propagation '[TextMap, Headers]
jaegerPropagation = Carrier _JaegerTextMap :& Carrier _JaegerHeaders :& RNil


_JaegerTextMap :: Prism' TextMap SpanContext
_JaegerTextMap = prism' fromCtx toCtx
  where
    fromCtx c = HashMap.fromList $
          ("uber-trace-id", review _UberTraceId c)
        : map (over _1 ("uberctx-" <>)) (view (ctxBaggage . to HashMap.toList) c)

    toCtx m =
          fmap (set ctxBaggage
                    (HashMap.filterWithKey (\k _ -> "uberctx-" `isPrefixOf` k) m))
        $ HashMap.lookup "uber-trace-id" m >>= preview _UberTraceId

_JaegerHeaders :: Prism' Headers SpanContext
_JaegerHeaders = _Headers' _JaegerTextMap

_UberTraceId :: Prism' Text SpanContext
_UberTraceId = prism' fromCtx toCtx
  where
    fromCtx c@SpanContext{..} =
        let traceid = view hexText ctxTraceID
            spanid  = view hexText ctxSpanID
            parent  = maybe mempty (view hexText) ctxParentSpanID
            flags   = if view (ctxSampled . re _IsSampled) c then "1" else "0"
         in Text.intercalate ":" [traceid, spanid, parent, flags]

    toCtx t =
        let sampledFlag = 1 :: Word
            debugFlag   = 2 :: Word
            shouldSample fs = fs .&. sampledFlag > 0 || fs .&. debugFlag > 0
         in case Text.split (==':') t of
                [traceid, spanid, _, flags] -> SpanContext
                    <$> preview _Hex (knownHex traceid)
                    <*> preview _Hex (knownHex spanid)
                    <*> pure Nothing
                    <*> either (const $ Just NotSampled)
                               (Just . view _IsSampled . shouldSample . fst)
                               (Text.decimal flags)
                    <*> pure mempty

                _ -> Nothing
