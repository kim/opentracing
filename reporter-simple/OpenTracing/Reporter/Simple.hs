{-# LANGUAGE OverloadedStrings #-}

module OpenTracing.Reporter.Simple (traceReport) where

import Control.Lens               hiding (Context, (.=))
import Control.Monad.IO.Class
import Data.Aeson                 hiding (Error)
import Data.Aeson.Encoding
import Data.ByteString.Lazy.Char8 (putStrLn)
import Data.Foldable              (toList)
import Data.Monoid
import GHC.Stack                  (prettyCallStack)
import Network.HTTP.Types         (statusCode)
import OpenTracing.Tracer.Simple
import OpenTracing.Types
import Prelude                    hiding (putStrLn)


traceReport :: MonadIO m  => FinishedSpan Context -> Tracer m ()
traceReport = liftIO . putStrLn . encodingToLazyByteString . spanE


spanE :: FinishedSpan Context -> Encoding
spanE s = pairs $
       pair "operation"  (text $ view spanOperation s)
    <> pair "start"      (utcTime $ view spanStart s)
    <> pair "duration"   (double . realToFrac $ view spanDuration s)
    <> pair "context"    (toEncoding $ view spanContext s)
    <> pair "references" (list refE . toList $ view spanRefs s)
    <> pair "tags"       (list tagE . toList $ view spanTags s)
    <> pair "logs"       (list logRecE . reverse $ view spanLogs s)

refE :: Reference Context -> Encoding
refE (ChildOf     ctx) = pairs . pair "child_of"     . toEncoding $ ctx
refE (FollowsFrom ctx) = pairs . pair "follows_from" . toEncoding $ ctx

tagE :: Tag -> Encoding
tagE t = pairs . pair (tagLabel t) $ case t of
    Component             x -> text x
    DbInstance            x -> text x
    DbStatement           x -> text x
    DbType                x -> text x
    DbUser                x -> text x
    Error                 x -> bool x
    HttpMethod            x -> string . show $ x
    HttpStatusCode        x -> int . statusCode $ x
    HttpUrl               x -> text x
    MessageBusDestination x -> text x
    PeerAddress           x -> text x
    PeerHostname          x -> text x
    PeerIPv4              x -> string . show $ x
    PeerIPv6              x -> string . show $ x
    PeerPort              x -> word64 . fromIntegral $ x
    PeerService           x -> text x
    SamplingPriority      x -> word8 x
    SpanKind              x -> text (spanKindLabel x)
    SomeTag             _ x -> text x

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
    LogField _ x -> string (show x)
