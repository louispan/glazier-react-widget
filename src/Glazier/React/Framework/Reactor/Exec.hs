{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Glazier.React.Framework.Reactor.Exec where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import qualified Control.Disposable as CD
import Control.Lens
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.RWSs.Strict
import Control.Monad.Trans.States.Strict
import Data.Diverse.Lens
import qualified Data.DList as DL
import Data.Foldable
import Data.Maybe
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Foreign.Export as J
import Glazier.React
import Glazier.React.Framework.MkId.Internal
import Glazier.React.Framework.Reactor
import Glazier.React.Framework.Scene
import Glazier.React.Framework.Window
import qualified JavaScript.Array as JA
import qualified JavaScript.Extras as JE
import Unsafe.Coerce

maybeExec :: (Monad m, AsFacet a c) => (a -> m b) -> c -> MaybeT m b
maybeExec k y = maybe empty pure (preview facet y) >>= (lift <$> k)

-- maybeExec' :: (Monad m, AsFacet (c' c) c) => (c' c -> m b) -> c -> MaybeT m b
-- maybeExec' = maybeExec

-- | Given an initializing state action, and an executor (eg. 'reactorExecutor'),
-- initialize and exec the resulting commands generated, then
-- return the TVars created.
-- It is the responsiblity of the caller to 'CD.dispose' of the Plan when the app finishes.
initReactor ::
    MonadIO m
    => (DL.DList c -> m ())
    -> s
    -> States (Scenario c s) ()
    -> m (TVar Plan, TVar s)
initReactor exec s ini = do
    planVar <- liftIO $ newTVarIO newPlan
    modelVar <- liftIO $ newTVarIO s
    -- run through the app initialization, and execute any produced commands
    xs <- liftIO . atomically $ tickState planVar modelVar ini
    exec xs
    pure (planVar, modelVar)

-- | Upate the world 'TVar' with the given action, and return the commands produced.
tickState :: TVar Plan -> TVar s -> States (Scenario c s) () -> STM (DL.DList c)
tickState planVar modelVar tick = do
    pln <- readTVar planVar
    mdl <- readTVar modelVar
    let (Scenario cs (Scene pln' mdl')) = execStates tick (Scenario mempty (Scene pln mdl))
    writeTVar modelVar mdl'
    writeTVar planVar pln'
    pure cs

-- newtype Wack = Wack (Which '[MkCallback1 (Scene Wack Int)])
-- type Wack w = Which '[MkCallback1 (Scene w Int)]
-- newtype Wock = Wock { unWock :: Wack Wock}
-- instance (AsFacet a (Wack Wock)) => AsFacet a Wock where
--     facet = iso unWock Wock . facet

-- Create a executor for all the core commands required by the framework
execReactor ::
    ( MonadIO m
    , AsFacet Rerender c
    , AsFacet (MkTick1 c) c
    , AsFacet (MkTick c) c
    , AsFacet MkShimCallbacks c
    , AsFacet CD.Disposable c
    )
    => (m () -> IO ()) -> (DL.DList c -> m ()) -> c -> m ()
execReactor runExec exec c = fmap (fromMaybe mempty) $ runMaybeT $
    maybeExec execRerender c
    <|> maybeExec execMkShimCallbacks c
    <|> maybeExec (execMkTick1 runExec exec) c
    <|> maybeExec (execMkTick runExec exec) c
    <|> maybeExec execDisposable c

-- | An example of using the "tieing" 'execReactor' with itself. Lazy haskell is awesome.
-- NB. This tied executor *only* runs the Reactor effects.
-- This version also 'traverse_' commands with parallel threads.
reactorExecutor ::
    ( MonadIO m
    , AsFacet Rerender c
    , AsFacet (MkTick1 c) c
    , AsFacet (MkTick c) c
    , AsFacet MkShimCallbacks c
    , AsFacet CD.Disposable c
    )
    => (m () -> IO ()) -> c -> m ()
reactorExecutor runExec = execReactor runExec
    (traverse_ $ liftIO . void . forkIO . runExec . reactorExecutor runExec)
    -- (traverse_ (reactorExecutor runExec))

-----------------------------------------------------------------

execMkTick1 ::
    ( MonadIO m
    )
    => (m () -> IO ())
    -> (DL.DList c -> m ())
    -> MkTick1 c
    -> m ()
execMkTick1 runExec exec (MkTick1 planVar modelVar goStrict goLazy k) = do
    -- create the IO action to run given the runExec and exec
    let f = handleEventM goStrict goLazy'
        goLazy' ma = case ma of
            -- trigger didn't produce anything useful
            Nothing -> pure mempty
            -- run state action using mvar
            Just a -> do
                -- Apply to result to the world state, and execute any produced commands
                xs <- atomically $ tickState planVar modelVar (goLazy a)
                runExec (exec xs)
    -- Apply to result to the continuation, and execute any produced commands
    xs <- liftIO $ atomically $ tickState planVar modelVar (k f)
    exec xs

execMkTick ::
    ( MonadIO m
    )
    => (m () -> IO ())
    -> (DL.DList c -> m ())
    -> MkTick c
    -> m ()
execMkTick runExec exec (MkTick planVar modelVar tick k) = do
    -- create the IO action to run given the runExec and exec
    let f = do
            xs <- atomically $ tickState planVar modelVar tick
            runExec (exec xs)
    -- Apply to result to the continuation, and execute any produced commands
    xs <- liftIO $ atomically $ tickState planVar modelVar (k f)
    exec xs


-----------------------------------------------------------------

execRerender ::
    ( MonadIO m
    )
    => Rerender -> m ()
execRerender (Rerender j p) = liftIO $ do
    -- This export is automatically cleaned up in ShimComponent
    -- react lifecycle handling.
    x <- J.export p
    js_setShimComponentFrame j x

-----------------------------------------------------------------

-- | Making multiple MkShimListeners for the same plan is a silent error and will be ignored.
execMkShimCallbacks ::
    MonadIO m
    => MkShimCallbacks
    -> m ()
execMkShimCallbacks (MkShimCallbacks planVar modelVar rndr) = do
    -- For efficiency, render uses the state exported into ShimComponent
    let doRender x = do
            -- unfortunately, GHCJS base doesn't provide a way to convert JSVal to Export
            ms <- J.derefExport (unsafeCoerce x)
            s <- case ms of
                -- fallback to reading from TVar
                Nothing -> atomically $ do
                    pln <- readTVar planVar
                    mdl <- readTVar modelVar
                    pure (Scene pln mdl)
                -- cached render export available
                Just s -> pure s
            let (mrkup, _) = execRWSs rndr s mempty
            JE.toJS <$> toElement mrkup
        doRef j = atomically $ modifyTVar' planVar (_componentRef .~ JE.fromJS j)
        doUpdated = join . atomically $ do
            pln <- readTVar planVar

            let x = doOnceOnUpdated pln
                y = doOnUpdated pln
            writeTVar planVar (pln & _doOnceOnUpdated .~ mempty)
            pure (x *> y)
        doListen ctx j = void $ runMaybeT $ do
            (gid, n) <- MaybeT $ pure $ do
                ctx' <- JE.fromJS ctx
                case JA.toList ctx' of
                        [gid', n'] -> do
                            gid'' <- GizmoId <$> JE.fromJS gid'
                            n'' <- JE.fromJS n'
                            Just (gid'', n'')
                        _ -> Nothing
            lift $ do
                hdl <- atomically $ do
                        pln <- readTVar planVar
                        let ((x, y), pln') = pln & (_gizmos.ix gid) go
                            go giz =
                                let (x', giz') = giz & (_oncelisteners.ix n) <<.~ mempty
                                    y' = view (_listeners.ix n) giz
                                in ((x', y'), giz')
                        writeTVar planVar pln'
                        pure (x *> y)
                hdl (JE.JSRep j)


    renderCb <- liftIO $ J.syncCallback1' doRender
    refCb <- liftIO $ J.syncCallback1 J.ContinueAsync doRef
    updatedCb <- liftIO $ J.syncCallback J.ContinueAsync doUpdated
    listenCb <- liftIO $ J.syncCallback2 J.ContinueAsync doListen
    liftIO $ atomically $ do
        pln <- readTVar planVar
        let ls = pln ^. _shimCallbacks
        case ls of
            -- shim listeners already created
            Just _ -> pure ()
            Nothing -> writeTVar planVar $ pln & _shimCallbacks .~
                (Just $ ShimCallbacks renderCb updatedCb refCb listenCb)

execDisposable ::
    MonadIO m
    => CD.Disposable
    -> m ()
execDisposable = liftIO . fromMaybe mempty . CD.runDisposable

execForkSTM ::
    MonadIO m
    => (m () -> IO ())
    -> (DL.DList c -> m ())
    -> ForkSTM c
    -> m ()
execForkSTM runExec exec (ForkSTM planVar modelVar go k) = liftIO $ void $ forkIO $ do
    a <- atomically go
    xs <- atomically $ tickState planVar modelVar (k a)
    liftIO $ runExec (exec xs)

#ifdef __GHCJS__

foreign import javascript unsafe
  "if ($1 && $1['setFrame']) { $1['setFrame']($2); }"
  js_setShimComponentFrame :: ComponentRef -> J.Export a -> IO ()

#else

js_setShimComponentFrame :: ComponentRef -> J.Export a -> IO ()
js_setShimComponentFrame _ _ = pure ()

#endif
