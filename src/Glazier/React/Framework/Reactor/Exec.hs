{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

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
import Data.Proxy
import Data.Tagged
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Foreign.Export as J
import Glazier.React
import Glazier.React.Framework.MkId.Internal
import Glazier.React.Framework.Reactor
import Glazier.React.Framework.Scene
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
    cs <- liftIO . atomically $ tickState planVar modelVar ini
    exec cs
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
    , AsFacet (TickState c) c
    , AsFacet (MkAction1 c) c
    , AsFacet (MkAction c) c
    , AsFacet MkShimCallbacks c
    , AsFacet CD.Disposable c
    )
    => (m () -> IO ()) -> (DL.DList c -> m ()) -> c -> m ()
execReactor runExec exec c = fmap (fromMaybe mempty) $ runMaybeT $
    maybeExec execRerender c
    <|> maybeExec (execTickState exec) c
    <|> maybeExec (execMkAction1 runExec exec) c
    <|> maybeExec (execMkAction runExec exec) c
    <|> maybeExec execMkShimCallbacks c
    <|> maybeExec execDisposable c

-- | An example of using the "tieing" 'execReactor' with itself. Lazy haskell is awesome.
-- NB. This tied executor *only* runs the Reactor effects.
-- This version also 'traverse_' commands with parallel threads.
reactorExecutor ::
    ( MonadIO m
    , AsFacet Rerender c
    , AsFacet (TickState c) c
    , AsFacet (MkAction1 c) c
    , AsFacet (MkAction c) c
    , AsFacet MkShimCallbacks c
    , AsFacet CD.Disposable c
    )
    => (m () -> IO ()) -> c -> m ()
reactorExecutor runExec = execReactor runExec
    (traverse_ $ liftIO . void . forkIO . runExec . reactorExecutor runExec)
    -- (traverse_ (reactorExecutor runExec))

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

execTickState ::
    ( MonadIO m
    , AsFacet Rerender c
    )
    => (DL.DList c -> m ())
    -> TickState c
    -> m ()
execTickState exec (TickState planVar modelVar tick) = do
    xs <- liftIO $ atomically $ tickState planVar modelVar (tick *> rerender)
    exec xs

execMkAction1 ::
    (m () -> IO ())
    -> (DL.DList c -> m ())
    -> MkAction1 c
    -> m ()
execMkAction1 runExec exec (MkAction1 goStrict goLazy k) = do
    -- create the IO action to run given the runExec and exec
    let f = handleEventM goStrict goLazy'
        goLazy' ma = case ma of
            -- trigger didn't produce anything useful
            Nothing -> pure mempty
            -- get and run the command given the trigger
            Just a -> runExec . exec . DL.singleton $ goLazy a
    -- Apply to result to the continuation, and execute any produced commands
    exec $ DL.singleton $ k f

execMkAction ::
    (m () -> IO ())
    -> (DL.DList c -> m ())
    -> MkAction c
    -> m ()
execMkAction runExec exec (MkAction c k) = do
    -- create the IO action to run given the runExec and exec
    let f = runExec . exec $ DL.singleton c
    -- Apply to result to the continuation, and execute any produced commands
    exec $ DL.singleton $ k f

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
            let ((`proxy` (Proxy @"Once")) -> x, (`proxy` (Proxy @"Every")) -> y) = doOnUpdated pln
            writeTVar planVar (pln & _doOnUpdated._1 .~ (Tagged @"Once" mempty))
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
                mhdl <- atomically $ do
                        pln <- readTVar planVar
                        let (mhdl, gs') = at gid go (pln ^. _gizmos)
                            go mg = case mg of
                                    Nothing -> (Nothing, Nothing)
                                    Just g -> let (ret, l) = at n go' (g ^. _listeners)
                                              in (ret, Just (g & _listeners .~ l))
                            go' ml = case ml of
                                    Nothing -> (Nothing, Nothing)
                                    Just ((`proxy` (Proxy @"Once")) -> x, y) ->
                                        ( Just (x *> (y `proxy` (Proxy @"Every")))
                                        , Just (Tagged @"Once" mempty, y))
                        writeTVar planVar (pln & _gizmos .~ gs')
                        pure mhdl
                case mhdl of
                    Nothing -> pure ()
                    Just hdl -> hdl (JE.JSRep j)

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
execForkSTM runExec exec (ForkSTM go k) = liftIO $ void $ forkIO $ do
    a <- atomically go
    let c = k a
    liftIO . runExec . exec $ DL.singleton c

#ifdef __GHCJS__

foreign import javascript unsafe
  "if ($1 && $1['setFrame']) { $1['setFrame']($2); }"
  js_setShimComponentFrame :: ComponentRef -> J.Export a -> IO ()

#else

js_setShimComponentFrame :: ComponentRef -> J.Export a -> IO ()
js_setShimComponentFrame _ _ = pure ()

#endif
