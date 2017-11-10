{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE StrictData             #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TupleSections          #-}

module OpenTracing.Span
    ( Span
    , newSpan

    , HasSpanFields

    , ActiveSpan
    , mkActive
    , modifyActiveSpan
    , readActiveSpan

    , FinishedSpan
    , defaultTraceFinish

    , spanContext
    , spanOperation
    , spanStart
    , spanTags
    , spanRefs
    , spanLogs
    , spanDuration

    , SpanOpts(..)
    , spanOpts

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
    , HasSampled(..)
    , sampled

    , Traced(..)
    )
where

import Control.Lens           hiding (op, pre)
import Control.Monad.IO.Class
import Data.Aeson             (ToJSON (..))
import Data.Aeson.Encoding    hiding (bool)
import Data.Bool              (bool)
import Data.Foldable
import Data.Hashable
import Data.IORef
import Data.Monoid
import Data.Text              (Text)
import Data.Time.Clock
import GHC.Generics           (Generic)
import OpenTracing.Log
import OpenTracing.Tags
import Prelude                hiding (span)


data Traced ctx a = Traced
    { tracedResult :: a
    , tracedSpan   :: FinishedSpan ctx
    }

data Sampled = NotSampled | Sampled
    deriving (Eq, Show, Read, Bounded, Enum, Generic)

instance Hashable Sampled

instance ToJSON Sampled where
    toJSON     = toJSON . fromEnum
    toEncoding = int . fromEnum

class HasSampled ctx where
    ctxSampled :: Lens' ctx Sampled

sampled :: Iso' Bool Sampled
sampled = iso (bool NotSampled Sampled) $ \case
    Sampled    -> True
    NotSampled -> False

data Reference ctx
    = ChildOf     { refCtx :: ctx }
    | FollowsFrom { refCtx :: ctx }
    deriving (Eq, Show, Generic)

instance Hashable ctx => Hashable (Reference ctx)

findParent :: Foldable t => t (Reference ctx) -> Maybe (Reference ctx)
findParent = foldl' go Nothing
  where
    go Nothing  y = Just y
    go (Just x) y = Just $ case prec x y of { LT -> y; _ -> x }

    prec (ChildOf     _) (FollowsFrom _) = GT
    prec (FollowsFrom _) (ChildOf     _) = LT
    prec _               _               = EQ


data SpanRefs ctx = SpanRefs
    { _refActiveParents :: [ActiveSpan   ctx]
    , _refPredecessors  :: [FinishedSpan ctx]
    , _refPropagated    :: [Reference    ctx]
    }

instance Monoid (SpanRefs ctx) where
    mempty = SpanRefs mempty mempty mempty

    (SpanRefs par pre pro) `mappend` (SpanRefs par' pre' pro') = SpanRefs
        { _refActiveParents = par <> par'
        , _refPredecessors  = pre <> pre'
        , _refPropagated    = pro <> pro'
        }

childOf :: ActiveSpan ctx -> SpanRefs ctx
childOf a = mempty { _refActiveParents = [a] }

followsFrom :: FinishedSpan ctx -> SpanRefs ctx
followsFrom a = mempty { _refPredecessors = [a] }

freezeRefs :: SpanRefs ctx -> IO [Reference ctx]
freezeRefs SpanRefs{..} = do
    a <- traverse (fmap (ChildOf . _sContext) . readActiveSpan) _refActiveParents
    let b = map (FollowsFrom . _fContext) _refPredecessors
    return $ a <> b <> _refPropagated


data SpanOpts ctx = SpanOpts
    { spanOptOperation :: Text
    , spanOptRefs      :: SpanRefs ctx
    , spanOptTags      :: [Tag]
    , spanOptSampled   :: Maybe Sampled
    -- ^ Force 'Span' to be sampled (or not).
    -- 'Nothing' denotes leave decision to 'Sampler' (the default)
    }

spanOpts :: Text -> SpanRefs ctx -> SpanOpts ctx
spanOpts op refs = SpanOpts
    { spanOptOperation = op
    , spanOptRefs      = refs
    , spanOptTags      = mempty
    , spanOptSampled   = Nothing
    }

data Span ctx = Span
    { _sContext   :: ctx
    , _sOperation :: Text
    , _sStart     :: UTCTime
    , _sTags      :: Tags
    , _sRefs      :: SpanRefs ctx
    , _sLogs      :: [LogRecord]
    }

newSpan
    :: ( MonadIO  m
       , Foldable t
       )
    => ctx
    -> Text
    -> SpanRefs ctx
    -> t Tag
    -> m (Span ctx)
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


newtype ActiveSpan ctx = ActiveSpan { fromActiveSpan :: IORef (Span ctx) }

mkActive :: Span ctx -> IO (ActiveSpan ctx)
mkActive = fmap ActiveSpan . newIORef

modifyActiveSpan :: ActiveSpan ctx -> (Span ctx -> Span ctx) -> IO ()
modifyActiveSpan ActiveSpan{fromActiveSpan} f =
    atomicModifyIORef' fromActiveSpan ((,()) . f)

readActiveSpan :: ActiveSpan ctx -> IO (Span ctx)
readActiveSpan = readIORef . fromActiveSpan


data FinishedSpan ctx = FinishedSpan
    { _fContext    :: ctx
    , _fOperation  :: Text
    , _fStart      :: UTCTime
    , _fDuration   :: NominalDiffTime
    , _fTags       :: Tags
    , _fReferences :: [Reference ctx]
    , _fLogs       :: [LogRecord]
    }

defaultTraceFinish :: MonadIO m => Span ctx -> m (FinishedSpan ctx)
defaultTraceFinish s = do
    (t,refs) <- liftIO $ (,) <$> getCurrentTime <*> freezeRefs (_sRefs s)
    pure FinishedSpan
        { _fContext    = _sContext s
        , _fOperation  = _sOperation s
        , _fStart      = _sStart s
        , _fDuration   = diffUTCTime t (_sStart s)
        , _fTags       = _sTags s
        , _fReferences = refs
        , _fLogs       = _sLogs s
        }

makeLenses ''Span
makeLenses ''FinishedSpan
makeLenses ''SpanRefs

class HasSpanFields a ctx | a -> ctx where
    spanContext   :: Lens' a ctx
    spanOperation :: Lens' a Text
    spanStart     :: Lens' a UTCTime
    spanTags      :: Lens' a Tags
    spanLogs      :: Lens' a [LogRecord]

instance HasSpanFields (Span ctx) ctx where
    spanContext   = sContext
    spanOperation = sOperation
    spanStart     = sStart
    spanTags      = sTags
    spanLogs      = sLogs

instance HasSpanFields (FinishedSpan ctx) ctx where
    spanContext   = fContext
    spanOperation = fOperation
    spanStart     = fStart
    spanTags      = fTags
    spanLogs      = fLogs


instance HasSampled ctx => HasSampled (Span ctx) where
    ctxSampled = spanContext . ctxSampled

instance HasSampled ctx => HasSampled (FinishedSpan ctx) where
    ctxSampled = spanContext . ctxSampled


spanDuration :: Lens' (FinishedSpan ctx) NominalDiffTime
spanDuration = fDuration

spanRefs :: Lens' (FinishedSpan ctx) [Reference ctx]
spanRefs = fReferences
