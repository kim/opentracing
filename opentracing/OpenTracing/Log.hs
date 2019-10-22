{-|
Module: OpenTracing.Log

Logs are structured data that occur over the lifetime of a span.
-}

{-# LANGUAGE GADTs              #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData         #-}
{-# LANGUAGE TemplateHaskell    #-}

module OpenTracing.Log
    ( LogRecord(..)
    , logTime
    , logFields

    , LogField(..)
    , logFieldLabel
    , logFieldEncoding
    , logFieldValue

    , LogFieldsFormatter
    , jsonAssoc
    , jsonMap
    )
where

import           Control.Exception
import           Control.Lens            hiding ((.=))
import           Data.Aeson
import qualified Data.Aeson.Encoding     as Encoding
import           Data.ByteString.Builder (Builder)
import           Data.Foldable
import           Data.List.NonEmpty      (NonEmpty)
import           Data.Text               (Text)
import           Data.Time.Clock
import           GHC.Stack
import qualified Data.Map.Strict as Map

-- | A single entry into a `Spans` logs. Occurs at a single time and contains multiple
-- (one or more) entries.
--
-- @since 0.1.0.0
data LogRecord = LogRecord
    { _logTime   :: UTCTime
    , _logFields :: NonEmpty LogField
    } deriving Show

-- | A piece of data in a `LogRecord`. Conceptually a key:value pair with a few
-- distinguished keys. More info about the distinguished keys in the [OpenTracing spec](https://github.com/opentracing/specification/blob/master/semantic_conventions.md#log-fields-table)
--
-- @since 0.1.0.0
data LogField where
    LogField :: Show      a => Text      -> a -> LogField
    -- ^ A generic key:value pair entry into a `LogRecord`
    --
    -- @since 0.1.0.0
    Event    ::                Text           -> LogField
    Message  ::                Text           -> LogField
    Stack    ::                CallStack      -> LogField
    ErrKind  ::                Text           -> LogField
    ErrObj   :: Exception e => e              -> LogField

deriving instance (Show LogField)

type LogFieldsFormatter = forall t. Foldable t => t LogField -> Builder

-- | A log formatter that encodes each `LogField` as a single JSON object.
--
-- >>> BS.hPutBuilder stdout $ jsonAssoc [Event "e", LogField @Text "key" "value"]
-- [{"event":"\"e\""},{"key":"\"value\""}]
--
-- @since 0.1.0.0
jsonAssoc :: LogFieldsFormatter
jsonAssoc = Encoding.fromEncoding . Encoding.list go . toList
  where
    go lf = Encoding.pairs $
        Encoding.pair (logFieldLabel lf) (logFieldEncoding lf)

-- | A log formatter that encodes each `LogField` as an entry in a shared JSON object
--
-- >>> BS.hPutBuilder stdout $ jsonMap  [Event "e", LogField @Text "key" "value"]
-- {"event":"e","key":"\"value\""}
--
-- @since 0.1.0.0
jsonMap :: LogFieldsFormatter
jsonMap
    = Encoding.fromEncoding
    . Encoding.dict Encoding.text id Map.foldrWithKey'
    . foldr' merge mempty
  where
    merge lf = Map.insert (logFieldLabel lf) (logFieldEncoding lf)

-- | Retrieve the label of a log field. Distinguished `LogField`s have predefined keys.
--
-- @since 0.1.0.0
logFieldLabel :: LogField -> Text
logFieldLabel (LogField x _) = x
logFieldLabel (Event      _) = "event"
logFieldLabel (Message    _) = "message"
logFieldLabel (Stack      _) = "stack"
logFieldLabel (ErrKind    _) = "error.kind"
logFieldLabel (ErrObj     _) = "error.object"

logFieldEncoding :: LogField -> Encoding
logFieldEncoding (LogField _ v) = Encoding.string $ show v
logFieldEncoding (Event      v) = Encoding.text v
logFieldEncoding (Message    v) = Encoding.text v
logFieldEncoding (Stack      v) = Encoding.string $ prettyCallStack v
logFieldEncoding (ErrKind    v) = Encoding.text v
logFieldEncoding (ErrObj     v) = Encoding.string $ show v

logFieldValue :: LogField -> Value
logFieldValue (LogField _ v) = toJSON $ show v
logFieldValue (Event      v) = toJSON v
logFieldValue (Message    v) = toJSON v
logFieldValue (Stack      v) = toJSON $ prettyCallStack v
logFieldValue (ErrKind    v) = toJSON v
logFieldValue (ErrObj     v) = toJSON $ show v


makeLenses ''LogRecord
