{-|
Module: OpenTracing.Reporting.Stdio

Logging reporters that emit spans to stdout, stderr and System.IO `Handles`.
-}
{-# LANGUAGE CPP               #-}
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
#if MIN_VERSION_aeson(2, 0, 0)
import qualified Data.Aeson.Key as Key
#endif
import Data.ByteString.Lazy.Char8 (hPutStrLn)
import Data.Foldable              (toList)
import GHC.Stack                  (prettyCallStack)
import OpenTracing.Log
import OpenTracing.Span
import System.IO                  (Handle, stderr, stdout)

-- | Implementation of `OpenTracing.Tracer.tracerReport` that logs `FinishedSpan`s to
-- stdout
stdoutReporter :: MonadIO m => FinishedSpan -> m ()
stdoutReporter = stdioReporter stdout

-- | Implementation of `OpenTracing.Tracer.tracerReport` that logs `FinishedSpan`s to
-- stderr
stderrReporter :: MonadIO m => FinishedSpan -> m ()
stderrReporter = stdioReporter stderr

-- | Implementation of `OpenTracing.Tracer.tracerReport` that logs `FinishedSpan`s to
-- a `Handle`.
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
logFieldE f = pairs . pair key $ case f of
    Event      x -> text x
    Message    x -> text x
    Stack      x -> string . prettyCallStack $ x
    ErrKind    x -> text x
    ErrObj     x -> string . show $ x
    LogField _ x -> string . show $ x
  where
#if MIN_VERSION_aeson(2, 0, 0)
    key = Key.fromText $ logFieldLabel f
#else
    key = logFieldLabel f
#endif
