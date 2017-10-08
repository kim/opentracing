{-# LANGUAGE GADTs              #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData         #-}
{-# LANGUAGE TemplateHaskell    #-}

module OpenTracing.Log
    ( LogRecord(..)
    , logTime
    , logFields

    , LogField(..)
    , logFieldLabel
    )
where

import Control.Exception
import Control.Lens
import Data.List.NonEmpty (NonEmpty)
import Data.Text          (Text)
import Data.Time.Clock
import GHC.Stack

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

logFieldLabel :: LogField -> Text
logFieldLabel (LogField x _) = x
logFieldLabel (Event      _) = "event"
logFieldLabel (Message    _) = "message"
logFieldLabel (Stack      _) = "stack"
logFieldLabel (ErrKind    _) = "error.kind"
logFieldLabel (ErrObj     _) = "error.object"


makeLenses ''LogRecord
