{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Network.HTTP.Client.OpenTracing
    ( httpTraced
    , httpTraced'
    )
where

import           Control.Lens                 (over, view)
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Semigroup               ((<>))
import qualified Data.Text                    as Text
import           Data.Text.Encoding           (decodeUtf8)
import           Network.HTTP.Client.Internal
    ( Manager (mModifyRequest, mModifyResponse)
    , Request (..)
    , getUri
    , responseStatus
    )
import           OpenTracing                  hiding (sampled)
import           Prelude                      hiding (span)

-- |
--
-- >>> :{
-- mgr <- newManager defaultManagerSettings
-- rq1 <- parseRequest "http://service1.local/foo"
-- rq2 <- parseRequest "http://service2.local/bar"
-- traced (spanOpts "toplevel" mempty) $ \parent -> do
--     rpc1 <- httpTraced (childOf parent) rq1 mgr httpLbs
--     rpc2 <- httpTraced (childOf parent <> followsFrom (tracedSpan rpc1))
--                        rq2 mgr httpLbs
--     return [tracedResult rpc1, tracedResult rpc2]
-- :}
--
httpTraced
    :: ( HasSampled  ctx
       , AsCarrier   HttpHeaders ctx ctx
       , HasTracing  r r ctx ctx
       , MonadReader r m
       , MonadIO     m
       )
    => SpanRefs ctx
    -> Request
    -> Manager
    -> (Request -> Manager -> IO a)
    -> m (Traced ctx a)
httpTraced refs req mgr f = do
    t <- view tracing
    liftIO $ httpTraced' t refs req mgr f

httpTraced'
    :: ( HasSampled  ctx
       , AsCarrier   HttpHeaders ctx ctx
       )
    => Tracing  ctx
    -> SpanRefs ctx
    -> Request
    -> Manager
    -> (Request -> Manager -> IO a)
    -> IO (Traced ctx a)
httpTraced' t refs req mgr f = do
    sampled <- fmap (view ctxSampled . refCtx) . findParent <$> freezeRefs refs

    let opt = SpanOpts
            { spanOptOperation = decodeUtf8 $ path req
            , spanOptRefs      = refs
            , spanOptSampled   = sampled
            , spanOptTags      =
                [ HttpMethod  (method req)
                , HttpUrl     (Text.pack . show $ getUri req)
                , PeerAddress (decodeUtf8 (host req))
                , SpanKind    RPCClient
                ]
            }

    traced' t opt $ \span ->
        let mgr' = modMgr span
         in f (req { requestManagerOverride = Just mgr' }) mgr'
  where
    modMgr span = mgr
        { mModifyRequest  = \rq ->
            inject rq . view spanContext <$> readActiveSpan span

        , mModifyResponse = \rs -> do
            modifyActiveSpan span $
                over spanTags (setTag (HttpStatusCode (responseStatus rs)))
            return rs
        }

    inject rq ctx = rq
        { requestHeaders = requestHeaders rq <> fromHttpHeaders (traceInject ctx)
        }
