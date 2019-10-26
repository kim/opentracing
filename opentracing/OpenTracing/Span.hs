{-|
Module: OpenTracing.Span

Data types and functions for manipulating [spans](https://github.com/opentracing/specification/blob/master/specification.md#span)
-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE StrictData             #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TupleSections          #-}

module OpenTracing.Span
    ( SpanContext(..)
    , ctxSampled
    , ctxBaggage

    , Span
    , newSpan

    , HasSpanFields

    , ActiveSpan
    , mkActive
    , modifyActiveSpan
    , readActiveSpan
    , addLogRecord
    , addLogRecord'
    , setBaggageItem
    , getBaggageItem

    , FinishedSpan
    , spanFinish

    , spanContext
    , spanOperation
    , spanStart
    , spanTags
    , spanRefs
    , spanLogs
    , spanDuration

    , SpanOpts
    , spanOpts
    , spanOptOperation
    , spanOptRefs
    , spanOptTags
    , spanOptSampled

    , Reference(..)
    , findParent

    , SpanRefs
    , refActiveParents
    , refPredecessors
    , refPropagated
    , childOf
    , followsFrom
    , freezeRefs

    , Sampled(..)
    , _IsSampled
    , sampled

    , Traced(..)
    )
where

import Control.Applicative
import Control.Lens           hiding (op, pre, (.=))
import Control.Monad.IO.Class
import Data.Aeson             (ToJSON (..), object, (.=))
import Data.Aeson.Encoding    (int, pairs)
import Data.Bool              (bool)
import Data.Foldable
import Data.HashMap.Strict    (HashMap, insert)
import Data.IORef
import Data.List.NonEmpty     (NonEmpty (..))
import Data.Semigroup
import Data.Text              (Text)
import Data.Time.Clock
import Data.Word
import OpenTracing.Log
import OpenTracing.Tags
import OpenTracing.Types
import Prelude                hiding (span)

-- | A `SpanContext` is the data that uniquely identifies a span
-- and the context in which it occurs. Spans occur in traces, which form
-- complete pictures of a computation, potentially across multiple machines.
--
-- @since 0.1.0.0
data SpanContext = SpanContext
    { ctxTraceID      :: TraceID
    -- ^ A trace identifier. Trace ids are globally unique
    , ctxSpanID       :: Word64
    -- ^ A span identifier. Span identifiers are unique to their trace.
    , ctxParentSpanID :: Maybe Word64
    -- ^ Spans without a parent are known as "root spans"
    , _ctxSampled     :: Sampled
    -- ^ Whether or not this span is going to be reported.
    , _ctxBaggage     :: HashMap Text Text
    -- ^ Baggage is arbitrary key:value pairs that cross process boundaries.
    }

instance ToJSON SpanContext where
    toEncoding SpanContext{..} = pairs $
           "trace_id" .= view hexText ctxTraceID
        <> "span_id"  .= view hexText ctxSpanID
        <> "sampled"  .= _ctxSampled
        <> "baggage"  .= _ctxBaggage

    toJSON SpanContext{..} = object
        [ "trace_id" .= view hexText ctxTraceID
        , "span_id"  .= view hexText ctxSpanID
        , "sampled"  .= _ctxSampled
        , "baggage"  .= _ctxBaggage
        ]

-- | A wrapper for a value that was produced by a traced computation.
--
-- @since 0.1.0.0
data Traced a = Traced
    { tracedResult :: a
    -- ^ The raw value produced
    , tracedSpan   :: ~FinishedSpan
    -- ^ The resulting span that was created
    }

-- | A datatype indicating whether a recorded span was sampled, i.e. whether or not
-- it will be reported. Traces are often sampled in high volume environments to keep
-- the amount of data generated manageable.
--
-- @since 0.1.0.0
data Sampled = NotSampled | Sampled
    deriving (Eq, Show, Read, Bounded, Enum)

instance ToJSON Sampled where
    toJSON     = toJSON . fromEnum
    toEncoding = int . fromEnum

_IsSampled :: Iso' Bool Sampled
_IsSampled = iso (bool NotSampled Sampled) $ \case
    Sampled    -> True
    NotSampled -> False

-- | A reference from one span to another. Spans can be related in two ways:
--
--   * `ChildOf` indicates that the parent span is dependent on the child span in order
--      to produce its own result.
--
--   * `FollowsFrom` indicates that there is no dependence relation, perhaps the
--      parent span spawned an asynchronous task.
--
-- More info in the [OpenTracing spec](https://github.com/opentracing/specification/blob/master/specification.md#references-between-spans)
--
-- @since 0.1.0.0
data Reference
    = ChildOf     { refCtx :: SpanContext }
    | FollowsFrom { refCtx :: SpanContext }

findParent :: Foldable t => t Reference -> Maybe Reference
findParent = foldl' go Nothing
  where
    go Nothing  y = Just y
    go (Just x) y = Just $ case prec x y of { LT -> y; _ -> x }

    prec (ChildOf     _) (FollowsFrom _) = GT
    prec (FollowsFrom _) (ChildOf     _) = LT
    prec _               _               = EQ

-- | The different references that a span can hold to other spans.
--
-- @since 0.1.0.0
data SpanRefs = SpanRefs
    { _refActiveParents :: [ActiveSpan  ]
    -- ^ Parent span references. `ActiveSpans` are still in progress (parent spans by
    -- definition depend on their children to complete)
    , _refPredecessors  :: [FinishedSpan]
    -- ^ Spans that this span `FollowsFrom`
    , _refPropagated    :: [Reference   ]
    -- ^ References that are propagated across process boundaries. Can be either parents
    -- or predecessors.
    }

instance Semigroup SpanRefs where
    (SpanRefs par pre pro) <> (SpanRefs par' pre' pro') = SpanRefs
        { _refActiveParents = par <> par'
        , _refPredecessors  = pre <> pre'
        , _refPropagated    = pro <> pro'
        }

instance Monoid SpanRefs where
    mempty  = SpanRefs mempty mempty mempty
    mappend = (<>)

-- | Create a `SpanRefs` containing the single refrence to a parent span.
--
-- @since 0.1.0.0
childOf :: ActiveSpan -> SpanRefs
childOf a = mempty { _refActiveParents = [a] }

-- | Create a `SpanRefs` containing the single refrence to a predecessor span.
--
-- @since 0.1.0.0
followsFrom :: FinishedSpan -> SpanRefs
followsFrom a = mempty { _refPredecessors = [a] }

-- | Convert `SpanRefs` (which may include the mutable `ActiveSpan`s) into
-- an immutable list of `Reference`s
--
-- @since 0.1.0.0
freezeRefs :: SpanRefs -> IO [Reference]
freezeRefs SpanRefs{..} = do
    a <- traverse (fmap (ChildOf . _sContext) . readActiveSpan) _refActiveParents
    let b = map (FollowsFrom . _fContext) _refPredecessors
    return $ a <> b <> _refPropagated

-- | `SpanOpts` is the metadata information about a span needed in order to start
-- measuring a span. This is the information that application code will provide in
-- order to indicate what a span is doing and how it related to other spans. More info
-- in the [OpenTracing spec](https://github.com/opentracing/specification/blob/master/specification.md#start-a-new-span)
--
-- @since 0.1.0.0
data SpanOpts = SpanOpts
    { _spanOptOperation :: Text
    -- ^ The span operation, a human-readable string which concisely represents the
    -- work done by the Span
    , _spanOptRefs      :: SpanRefs
    -- ^ Zero or more references to related spans. Zero references indicates that
    -- a span is a root span and should be given a new trace ID.
    , _spanOptTags      :: [Tag]
    -- ^ Tags describing the work done by the span in more detail than the operation
    -- provides.
    , _spanOptSampled   :: Maybe Sampled
    -- ^ Force 'Span' to be sampled (or not).
    -- 'Nothing' denotes leave decision to 'Sampler' (the default)
    }

-- | Create a new `SpanOpts` with the minimal amount of required information.
--
-- @since 0.1.0.0
spanOpts :: Text -> SpanRefs -> SpanOpts
spanOpts op refs = SpanOpts
    { _spanOptOperation = op
    , _spanOptRefs      = refs
    , _spanOptTags      = mempty
    , _spanOptSampled   = Nothing
    }

-- | `Span` is a span that has been started (but not finished). See the [OpenTracing spec](https://github.com/opentracing/specification/blob/master/specification.md#span) for
-- more info
--
-- @since 0.1.0.0
data Span = Span
    { _sContext   :: SpanContext
    -- ^ The context in which a span occurs
    , _sOperation :: Text
    -- ^ The operation that describes a span (see `SpanOpts` for more info)
    , _sStart     :: UTCTime
    -- ^ The time that the span started
    , _sTags      :: Tags
    -- ^ Tags describing the span in more detail than the operation.
    , _sRefs      :: SpanRefs
    -- ^ References the span holds to other spans
    , _sLogs      :: [LogRecord]
    -- ^ Structured data the describe events over the lifetime of the span
    }

-- | Create a new `Span` with the provided info. The created `Span` will have a start
-- time equal to the system time when `newSpan` is called.
--
-- @since 0.1.0.0
newSpan
    :: ( MonadIO  m
       , Foldable t
       )
    => SpanContext
    -> Text
    -> SpanRefs
    -> t Tag
    -> m Span
newSpan ctx op refs ts = do
    t <- liftIO getCurrentTime
    pure Span
        { _sContext   = ctx
        , _sOperation = op
        , _sStart     = t
        , _sTags      = foldMap (`setTag` mempty) ts
        , _sRefs      = refs
        , _sLogs      = mempty
        }

-- | A mutable `Span` that is currently being recorded.
--
-- @since 0.1.0.0
newtype ActiveSpan = ActiveSpan { fromActiveSpan :: IORef Span }

-- | @since 0.1.0.0
mkActive :: Span -> IO ActiveSpan
mkActive = fmap ActiveSpan . newIORef

-- | @since 0.1.0.0
modifyActiveSpan :: ActiveSpan -> (Span -> Span) -> IO ()
modifyActiveSpan ActiveSpan{fromActiveSpan} f =
    atomicModifyIORef' fromActiveSpan ((,()) . f)

-- | @since 0.1.0.0
readActiveSpan :: ActiveSpan -> IO Span
readActiveSpan = readIORef . fromActiveSpan

-- | A span that has finished executing.
--
-- @since 0.1.0.0
data FinishedSpan = FinishedSpan
    { _fContext   :: SpanContext
    , _fOperation :: Text
    , _fStart     :: UTCTime
    , _fDuration  :: NominalDiffTime
    , _fTags      :: Tags
    , _fRefs      :: [Reference]
    , _fLogs      :: [LogRecord]
    }

-- | Convert an unfinished `Span` into a `FinishedSpan`
--
-- @since 0.1.0.0
spanFinish :: MonadIO m => Span -> m FinishedSpan
spanFinish s = do
    (t,refs) <- liftIO $ liftA2 (,) getCurrentTime (freezeRefs (_sRefs s))
    pure FinishedSpan
        { _fContext   = _sContext s
        , _fOperation = _sOperation s
        , _fStart     = _sStart s
        , _fDuration  = diffUTCTime t (_sStart s)
        , _fTags      = _sTags s
        , _fRefs      = refs
        , _fLogs      = _sLogs s
        }

makeLenses ''SpanContext
makeLenses ''SpanOpts
makeLenses ''Span
makeLenses ''FinishedSpan
makeLenses ''SpanRefs

class HasSpanFields a where
    spanContext   :: Lens' a SpanContext
    spanOperation :: Lens' a Text
    spanStart     :: Lens' a UTCTime
    spanTags      :: Lens' a Tags
    spanLogs      :: Lens' a [LogRecord]

instance HasSpanFields Span where
    spanContext   = sContext
    spanOperation = sOperation
    spanStart     = sStart
    spanTags      = sTags
    spanLogs      = sLogs

instance HasSpanFields FinishedSpan where
    spanContext   = fContext
    spanOperation = fOperation
    spanStart     = fStart
    spanTags      = fTags
    spanLogs      = fLogs

class HasSampled a where
    sampled :: Lens' a Sampled

instance HasSampled Sampled where
    sampled = id

instance HasSampled SpanContext where
    sampled = ctxSampled

instance HasSampled Span where
    sampled = spanContext . sampled

instance HasSampled FinishedSpan where
    sampled = spanContext . sampled


class HasRefs s a | s -> a where
    spanRefs :: Lens' s a

instance HasRefs Span SpanRefs where
    spanRefs = sRefs

instance HasRefs FinishedSpan [Reference] where
    spanRefs = fRefs


spanDuration :: Lens' FinishedSpan NominalDiffTime
spanDuration = fDuration

-- | Log structured data to an `ActiveSpan`. More info in the [OpenTracing spec](https://github.com/opentracing/specification/blob/master/specification.md#log-structured-data)
--
-- @since 0.1.0.0
addLogRecord :: ActiveSpan -> LogField -> IO ()
addLogRecord s f = addLogRecord' s f []

addLogRecord' :: ActiveSpan -> LogField -> [LogField] -> IO ()
addLogRecord' s f fs = do
    t <- getCurrentTime
    modifyActiveSpan s $
        over spanLogs (LogRecord t (f :| fs):)


setBaggageItem :: ActiveSpan -> Text -> Text -> IO ()
setBaggageItem s k v = modifyActiveSpan s $
    over (spanContext . ctxBaggage) (insert k v)

getBaggageItem :: ActiveSpan -> Text -> IO (Maybe Text)
getBaggageItem s k = view (spanContext . ctxBaggage . at k) <$> readActiveSpan s
