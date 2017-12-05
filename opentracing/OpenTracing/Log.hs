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


data LogRecord = LogRecord
    { _logTime   :: UTCTime
    , _logFields :: NonEmpty LogField
    } deriving Show

data LogField where
    LogField :: Show      a => Text      -> a -> LogField
    Event    ::                Text           -> LogField
    Message  ::                Text           -> LogField
    Stack    ::                CallStack      -> LogField
    ErrKind  ::                Text           -> LogField
    ErrObj   :: Exception e => e              -> LogField

deriving instance (Show LogField)

type LogFieldsFormatter = forall t. Foldable t => t LogField -> Builder

jsonAssoc :: LogFieldsFormatter
jsonAssoc = Encoding.fromEncoding . Encoding.list go . toList
  where
    go lf = Encoding.pairs $
        Encoding.pair (logFieldLabel lf) (logFieldEncoding lf)

jsonMap :: LogFieldsFormatter
jsonMap
    = Encoding.fromEncoding
    . Encoding.dict Encoding.text id Map.foldrWithKey'
    . foldr' merge mempty
  where
    merge lf = Map.insert (logFieldLabel lf) (logFieldEncoding lf)

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
