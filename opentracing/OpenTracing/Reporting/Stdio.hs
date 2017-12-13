{-# LANGUAGE OverloadedStrings #-}

module OpenTracing.Reporting.Stdio
    ( stdoutReporter
    , stderrReporter
    , stdioReporter
    )
where

import Control.Lens               (view)
import Control.Monad.IO.Class
import Data.Aeson                 (toEncoding)
import Data.Aeson.Encoding
import Data.ByteString.Lazy.Char8 (hPutStrLn)
import Data.Foldable              (toList)
import Data.Semigroup             ((<>))
import GHC.Stack                  (prettyCallStack)
import OpenTracing.Log
import OpenTracing.Span
import System.IO                  (Handle, stderr, stdout)


stdoutReporter :: MonadIO m => FinishedSpan -> m ()
stdoutReporter = stdioReporter stdout

stderrReporter :: MonadIO m => FinishedSpan -> m ()
stderrReporter = stdioReporter stderr

stdioReporter :: MonadIO m => Handle -> FinishedSpan -> m ()
stdioReporter h = liftIO . hPutStrLn h . encodingToLazyByteString . spanE


spanE :: FinishedSpan -> Encoding
spanE s = pairs $
       pair "operation"  (text $ view spanOperation s)
    <> pair "start"      (utcTime $ view spanStart s)
    <> pair "duration"   (double . realToFrac $ view spanDuration s)
    <> pair "context"    (toEncoding $ view spanContext s)
    <> pair "references" (list refE . toList $ view spanRefs s)
    <> pair "tags"       (toEncoding $ view spanTags s)
    <> pair "logs"       (list logRecE . reverse $ view spanLogs s)

refE :: Reference -> Encoding
refE (ChildOf     ctx) = pairs . pair "child_of"     . toEncoding $ ctx
refE (FollowsFrom ctx) = pairs . pair "follows_from" . toEncoding $ ctx

logRecE :: LogRecord -> Encoding
logRecE r = pairs $
       pair "time"   (utcTime (view logTime r))
    <> pair "fields" (list logFieldE . toList $ view logFields r)

logFieldE :: LogField -> Encoding
logFieldE f = pairs . pair (logFieldLabel f) $ case f of
    Event      x -> text x
    Message    x -> text x
    Stack      x -> string . prettyCallStack $ x
    ErrKind    x -> text x
    ErrObj     x -> string . show $ x
    LogField _ x -> string . show $ x
