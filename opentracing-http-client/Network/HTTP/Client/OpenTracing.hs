{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Network.HTTP.Client.OpenTracing where

import           Control.Lens                 (over, view)
import           Control.Monad.IO.Class       (MonadIO)
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
-- traced (spanOpts "toplevel" mempty) $ \parent -> do
--     rpc1 <- httpTraced tracing (childOf parent) req mgr httpLbs
--     rpc2 <- httpTraced tracing
--                        (childOf parent <> followsFrom (tracedSpan rpc1))
--                        req mgr $ \r m ->
--                 withResponse r m brConsume
--     return [tracedResult rpc1, tracedResult rpc2]
-- :}
httpTraced
    :: ( HasSampled ctx
       , AsCarrier  HttpHeaders ctx ctx
       )
    => Tracing ctx MonadIO
    -> SpanRefs ctx
    -> Request
    -> Manager
    -> (Request -> Manager -> IO a)
    -> IO (Traced ctx a)
httpTraced tracing refs req mgr f = do
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

    traced' tracing opt $ \span ->
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
