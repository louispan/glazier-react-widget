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
    ( MonadIO m
    , AsFacet [c] c
    )
    => (c -> m ())
    -> s
    -> States (Scenario c s) ()
    -> m (TVar Plan, TVar s)
initReactor exec s ini = do
    plnVar <- liftIO $ newTVarIO newPlan
    mdlVar <- liftIO $ newTVarIO s
    -- run through the app initialization, and execute any produced commands
    cs <- liftIO . atomically $ tickState plnVar mdlVar ini
    exec (cmd' $ DL.toList cs)
    pure (plnVar, mdlVar)

-- | Upate the world 'TVar' with the given action, and return the commands produced.
tickState :: TVar Plan -> TVar s -> States (Scenario c s) () -> STM (DL.DList c)
tickState plnVar mdlVar tick = do
    pln <- readTVar plnVar
    mdl <- readTVar mdlVar
    let (Scenario cs (Scene pln' mdl')) = execStates tick (Scenario mempty (Scene pln mdl))
    writeTVar mdlVar mdl'
    writeTVar plnVar pln'
    pure cs

-- type Wack w = Which '[Rerender, (), DL.DList w]
-- newtype Wock = Wock { unWock :: Wack Wock}
-- instance (AsFacet a (Wack Wock)) => AsFacet a Wock where
--     facet = iso unWock Wock . facet

-- | Create a executor for all the core commands required by the framework
execReactor ::
    ( MonadIO m
    , AsReactor c
    )
    => (m () -> IO ()) -> (c -> m ()) -> c -> MaybeT m ()
execReactor runExec exec c =
    maybeExec (execCommands runExec exec) c
    <|> maybeExec execRerender c
    <|> maybeExec (execTickState exec) c
    <|> maybeExec (execMkAction1 runExec exec) c
    <|> maybeExec (execMkAction runExec exec) c
    <|> maybeExec execMkShimCallbacks c
    <|> maybeExec execDisposable c
    <|> maybeExec (execForkSTM runExec exec) c

-- -- | An example of using the "tieing" 'execReactor' with itself. Lazy haskell is awesome.
-- -- NB. This tied executor *only* runs the Reactor effects.
-- -- You would probably want to "tie" at least 'execReactor', 'execJavascript', 'execHtmlElement'
-- reactorExecutor ::
--     ( MonadIO m
--     , AsReactor c
--     )
--     => (m () -> IO ()) -> c -> m ()
-- reactorExecutor runExec = fmap (fromMaybe mempty) . runMaybeT .
--     execReactor runExec (reactorExecutor runExec)

-----------------------------------------------------------------

-- execte a list of commands concurrently
execCommands :: MonadIO m => (m () -> IO ()) -> (c -> m ()) -> [c] -> m ()
execCommands runExec exec = traverse_ (liftIO . void . forkIO . runExec . exec)

execRerender ::
    ( MonadIO m
    )
    => Rerender -> m ()
execRerender (Rerender j p) = liftIO $ do
    -- This export is automatically cleaned up in ShimComponent
    -- react lifecycle handling.
    x <- J.export p
    js_setShimComponentFrame j x

-- FIXME: How to enforce FIFO for concurrent threads modifying the same TMVar?
-- Otherwise we will get strange updates - ie an earlier update overriding later update.
execTickState ::
    ( MonadIO m
    , AsFacet Rerender c
    , AsFacet [c] c
    )
    => (c -> m ())
    -> TickState c
    -> m ()
execTickState exec (TickState plnVar mdlVar tick) = do
    cs <- liftIO $ atomically $ tickState plnVar mdlVar (tick *> rerender)
    exec (cmd' $ DL.toList cs)

execMkAction1 ::
    (m () -> IO ())
    -> (c -> m ())
    -> MkAction1 c
    -> m ()
execMkAction1 runExec exec (MkAction1 goStrict goLazy k) = do
    -- create the IO action to run given the runExec and exec
    let f = handleEventM goStrict goLazy'
        goLazy' ma = case ma of
            -- trigger didn't produce anything useful
            Nothing -> pure mempty
            -- get and run the command given the trigger
            Just a -> runExec . exec $ goLazy a
    -- Apply to result to the continuation, and execute any produced commands
    exec $ k f

execMkAction ::
    (m () -> IO ())
    -> (c -> m ())
    -> MkAction c
    -> m ()
execMkAction runExec exec (MkAction c k) = do
    -- create the IO action to run given the runExec and exec
    let f = runExec $ exec c
    -- Apply to result to the continuation, and execute any produced commands
    exec $ k f

-- | Making multiple MkShimListeners for the same plan is a silent error and will be ignored.
execMkShimCallbacks ::
    MonadIO m
    => MkShimCallbacks
    -> m ()
execMkShimCallbacks (MkShimCallbacks plnVar mdlVar rndr) = do
    -- For efficiency, render uses the state exported into ShimComponent
    let doRender x = do
            -- unfortunately, GHCJS base doesn't provide a way to convert JSVal to Export
            mscn <- J.derefExport (unsafeCoerce x)
            scn <- case mscn of
                -- fallback to reading from TVar
                Nothing -> atomically $ do
                    pln <- readTVar plnVar
                    mdl <- readTVar mdlVar
                    pure (Scene pln mdl)
                -- cached render export available
                Just scn -> pure scn
            let (mrkup, _) = execRWSs rndr scn mempty
            JE.toJS <$> toElement mrkup
        doRef j = atomically $ modifyTVar' plnVar (_componentRef .~ JE.fromJS j)
        doUpdated = join . atomically $ do
            pln <- readTVar plnVar
            let ((`proxy` (Proxy @"Once")) -> x, (`proxy` (Proxy @"Every")) -> y) = doOnUpdated pln
            writeTVar plnVar (pln & _doOnUpdated._1 .~ (Tagged @"Once" mempty))
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
                        pln <- readTVar plnVar
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
                        writeTVar plnVar (pln & _gizmos .~ gs')
                        pure mhdl
                case mhdl of
                    Nothing -> pure ()
                    Just hdl -> hdl (JE.JSRep j)

    renderCb <- liftIO $ J.syncCallback1' doRender
    refCb <- liftIO $ J.syncCallback1 J.ContinueAsync doRef
    updatedCb <- liftIO $ J.syncCallback J.ContinueAsync doUpdated
    listenCb <- liftIO $ J.syncCallback2 J.ContinueAsync doListen
    liftIO $ atomically $ do
        pln <- readTVar plnVar
        let ls = pln ^. _shimCallbacks
        case ls of
            -- shim listeners already created
            Just _ -> pure ()
            Nothing -> writeTVar plnVar $ pln & _shimCallbacks .~
                (Just $ ShimCallbacks renderCb updatedCb refCb listenCb)

execDisposable ::
    MonadIO m
    => CD.Disposable
    -> m ()
execDisposable = liftIO . fromMaybe mempty . CD.runDisposable

execForkSTM ::
    MonadIO m
    => (m () -> IO ())
    -> (c -> m ())
    -> ForkSTM c
    -> m ()
execForkSTM runExec exec (ForkSTM go k) = liftIO $ void $ forkIO $ do
    a <- atomically go
    let c = k a
    liftIO . runExec $ exec c

#ifdef __GHCJS__

foreign import javascript unsafe
  "if ($1 && $1['setFrame']) { $1['setFrame']($2); }"
  js_setShimComponentFrame :: ComponentRef -> J.Export a -> IO ()

#else

js_setShimComponentFrame :: ComponentRef -> J.Export a -> IO ()
js_setShimComponentFrame _ _ = pure ()

#endif
