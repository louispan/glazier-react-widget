{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Glazier.React.Framework.Reactor.Exec where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar.Extras
import qualified Control.Disposable as CD
import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.RWSs.Strict
import Control.Monad.Trans.States.Strict
import Data.Diverse.Lens
import qualified Data.DList as DL
import Data.Foldable
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Proxy
import Data.Semigroup
import qualified GHC.Generics as G
import qualified GHCJS.Foreign.Callback as J
import Glazier.React
import Glazier.React.Framework.MkId
import Glazier.React.Framework.Reactor
import Glazier.React.Framework.Scene
import qualified JavaScript.Extras as JE
import qualified JavaScript.Object as JO

maybeExec :: forall a x m b. (Monad m, AsFacet a x) => (a -> m b) -> x -> MaybeT m b
maybeExec k y = maybe empty pure (preview facet y) >>= (lift <$> k)

-- | Given an initializing state action, and an executor (eg. 'reactorExecutor'),
-- initialize and exec the resulting commands generated.
-- It will create @TMVar (Scene x s)@ that will contain the world state
-- and a @TVar (Scene x s)@ with copy the latest state
-- (for non-blocking reads and rendering) and add it to the environment
-- using the given @addEnv@
-- NB. It the responsibility of the caller of this function to call 'disposableWorld'
-- when the application is finished.
-- It is expected that @m@ is a @MonadReader r m@
-- You can also use the @TMVar (Scene x s)@ to modify the world state in other threads
-- from other events (remember to update @TVar (Scene x s) with any changes).
initReactor ::
    ( MonadIO n
    , MonadState Int n
    )
    => States (Scenario c t) ()
    -> (TMVar (Scenario c t) -> TVar (Scene t) -> env -> r)
    -> (m () -> r -> IO ())
    -> (DL.DList c -> m ())
    -> env
    -> t
    -> n ()
initReactor ini addEnv runExec exec env t = do
    -- make state var
    pid <- mkPlanId "App"
    let t' = Scenario mempty (Scene (newPlan pid) t)
    liftIO $ do
        world <- newTMVarIO t'
        frame <- newTVarIO (t' ^. _scene)
        -- run through the app initialization
        cs <- atomically $ runAction world frame ini
        -- now go through and evaluate the commands
        -- This will include making callbacks which will execute on other threads
        -- completely consume all commands
        runExec (exec cs) (addEnv world frame env)

-- | Get the 'CD.Disposable' required to cleanup the world
-- without modifying the world.
-- NB. Because other theads may modify the world, this is only
-- guaranteed to contain all the required disposables if other threads have stopped.
-- After cleanup the JS listener callbacks will now result in exceptions
-- so cleanup should only be done after the reactComponents have been removed
-- from the DOM.
disposableWorld :: TMVar (Scenario c t) -> STM CD.Disposable
disposableWorld world = do
    -- get the disposables for the plan retrieve the final state
    t <- readTMVar world
    evalStatesT planDisposables t
  where
    planDisposables :: Monad m => StatesT (Scenario c t) m CD.Disposable
    planDisposables = CD.dispose <$> use _plan

-- | Upate the world 'TMVar' and backbuffer 'TVar' with a given action, and return the commands produced.
runAction :: TMVar (Scenario c t) -> TVar (Scene t) -> States (Scenario c t) () -> STM (DL.DList c)
runAction world frame action = do
    t <- takeTMVar world
    let (cs, t') = runStates (action *> takeCommands) t
    putTMVar world t'
    writeTVar frame (t' ^. _scene)
    pure cs
  where
    takeCommands = do
        cs <- use _commands
        _commands .= mempty
        pure cs

-- newtype Wack = Wack (Which '[MkCallback1 (Scene Wack Int)])
-- type Wack w = Which '[MkCallback1 (Scene w Int)]
-- newtype Wock = Wock { unWock :: Wack Wock}
-- instance (AsFacet a (Wack Wock)) => AsFacet a Wock where
--     facet = iso unWock Wock . facet

-- Create a executor for all the core commands required by the framework
execReactor :: forall t c r m.
    ( MonadIO m
    , MonadReader r m
    , AsFacet Rerender c
    , AsFacet (MkCallback1 c t) c
    , AsFacet (MkEveryOnUpdatedCallback c t) c
    , AsFacet (MkOnceOnUpdatedCallback c t) c
    , AsFacet (MkShimListeners c t) c
    , AsFacet (ForkSTMAction c t) c
    , HasItem' (TMVar (M.Map PlanId (EveryOnUpdated c t ()))) r
    , HasItem' (TMVar (M.Map PlanId (OnceOnUpdated c t ()))) r
    , HasItem' (TMVar (Scenario c t)) r
    , HasItem' (TVar (Scene t)) r
    )
    => Proxy t -> (m () -> r -> IO ()) -> (DL.DList c -> m ()) -> c -> m ()
execReactor _ runExec exec c = fmap (fromMaybe mempty) $ runMaybeT $
    maybeExec execRerender c
    <|> maybeExec @(MkCallback1 c t) (execMkCallback1 runExec exec) c
    <|> maybeExec @(MkEveryOnUpdatedCallback c t) execEveryOnUpdatedCallback c
    <|> maybeExec @(MkOnceOnUpdatedCallback c t) execOnceOnUpdatedCallback c
    <|> maybeExec @(MkShimListeners c t) (execMkShimListeners runExec exec) c
    <|> maybeExec @(ForkSTMAction c t) (execForkSTMAction runExec exec) c

-- | An example of using the "tieing" 'execReactor' with itself. Lazy haskell is awesome.
-- NB. This tied executor *only* runs the Reactor effects.
reactorExecutor :: forall t c r m.
    ( MonadIO m
    , MonadReader r m
    , AsFacet Rerender c
    , AsFacet (MkCallback1 c t) c
    , AsFacet (MkEveryOnUpdatedCallback c t) c
    , AsFacet (MkOnceOnUpdatedCallback c t) c
    , AsFacet (MkShimListeners c t) c
    , AsFacet (ForkSTMAction c t) c
    , HasItem' (TMVar (M.Map PlanId (EveryOnUpdated c t ()))) r
    , HasItem' (TMVar (M.Map PlanId (OnceOnUpdated c t ()))) r
    , HasItem' (TMVar (Scenario c t)) r
    , HasItem' (TVar (Scene t)) r
    )
    => Proxy t -> (m () -> r -> IO ()) -> c -> m ()
reactorExecutor p runExec c = execReactor p runExec (traverse_ (reactorExecutor p runExec)) c

-----------------------------------------------------------------

execRerender ::
    ( MonadIO m
    )
    => Rerender -> m ()
execRerender (Rerender j i) = do
    liftIO $ js_setComponentStates (JE.fromProperties [("frameNum", JE.toJSR i)]) j
    pure mempty

-----------------------------------------------------------------

execMkCallback1 ::
    ( MonadIO m
    , MonadReader r m
    , HasItem' (TMVar (Scenario c t)) r
    , HasItem' (TVar (Scene t)) r
    )
    => (m () -> r -> IO ())
    -> (DL.DList c -> m ())
    -> MkCallback1 c t
    -> m ()
execMkCallback1 runExec exec (MkCallback1 goStrict goLazy k) = do
    world <- view item' <$> ask
    frame <- view item' <$> ask
    env <- ask
    let f = handleEventM goStrict goLazy'
        goLazy' ma = case ma of
            -- trigger didn't produce anything useful
            Nothing -> pure mempty
            -- run state action using mvar
            Just a -> do
                -- Apply to result to the world state, and execute any produced commands
                xs <- atomically $ runAction world frame (goLazy a)
                runExec (exec xs) env
    -- Apply to result to the world state, and execute any produced commands
    xs <- liftIO $ do
        cb <- J.syncCallback1 J.ContinueAsync (f . JE.JSRep)
        atomically $ runAction world frame (k cb)
    exec xs

-----------------------------------------------------------------

newtype OnceOnUpdated c t a = OnceOnUpdated (States (Scenario c t) a)
    deriving (G.Generic, Functor, Applicative, Monad, Semigroup, Monoid)

execOnceOnUpdatedCallback :: forall c t m r.
    ( MonadIO m
    , MonadReader r m
    , HasItem' (TMVar (M.Map PlanId (OnceOnUpdated c t ()))) r
    )
    => MkOnceOnUpdatedCallback c t
    -> m ()
execOnceOnUpdatedCallback (MkOnceOnUpdatedCallback pid action) = do
    v <- view (item' @(TMVar (M.Map PlanId (OnceOnUpdated c t ())))) <$> ask
    liftIO . atomically . modifyTMVar_ v $ pure . over (at pid) (Just . (*> (OnceOnUpdated action)) . fromMaybe (pure ()))

-----------------------------------------------------------------

newtype EveryOnUpdated c t a = EveryOnUpdated (States (Scenario c t) a)
    deriving (G.Generic, Functor, Applicative, Monad, Semigroup, Monoid)

execEveryOnUpdatedCallback :: forall c t m r.
    ( MonadIO m
    , MonadReader r m
    , HasItem' (TMVar (M.Map PlanId (EveryOnUpdated c t ()))) r
    )
    => MkEveryOnUpdatedCallback c t
    -> m ()
execEveryOnUpdatedCallback (MkEveryOnUpdatedCallback pid action) = do
    v <- view (item' @(TMVar (M.Map PlanId (EveryOnUpdated c t ())))) <$> ask
    liftIO . atomically . modifyTMVar_ v $ pure . over (at pid) (Just . (*> (EveryOnUpdated action)) . fromMaybe (pure ()))

-----------------------------------------------------------------

-- | Making multiple MkShimListeners for the same plan is a silent error and will be ignored.
execMkShimListeners :: forall c t m r.
    ( MonadIO m
    , MonadReader r m
    , HasItem' (TMVar (Scenario c t)) r
    , HasItem' (TVar (Scene t)) r
    , HasItem' (TMVar (M.Map PlanId (OnceOnUpdated c t ()))) r
    , HasItem' (TMVar (M.Map PlanId (EveryOnUpdated c t ()))) r
    )
    => (m () -> r -> IO ())
    -> (DL.DList c -> m ())
    -> MkShimListeners c t
    -> m ()
execMkShimListeners runExec exec (MkShimListeners pid (Traversal myPlan) wind) = do
    world <- view item' <$> ask
    frame <- view item' <$> ask
    env <- ask
    onceOnUpdated <- view (item' @(TMVar (M.Map PlanId (OnceOnUpdated c t ())))) <$> ask
    everyOnUpdated <- view (item' @(TMVar (M.Map PlanId (EveryOnUpdated c t ())))) <$> ask
    -- render reads from the backbuffer 'TVar' so it doesn't block
    let doRender = do
            frm <- atomically $ readTVar frame
            let (as, _) = execRWSs wind frm mempty
            JE.toJS <$> toElement as
        doRef j = atomically $ do
            let c = JE.fromJS j
            modifyTMVar_ world $ pure . (myPlan._componentRef .~ c)
        doUpdated = do
            -- get the once only action to run and clear it
            OnceOnUpdated o <- atomically $ maybeModifyTMVar onceOnUpdated $ \as -> do
                let o = preview (ix pid) as
                case o of
                    Nothing -> pure (Nothing, mempty)
                    Just o' -> pure (Just $ as & at pid .~ Nothing, o')
            -- get the onEveryUpdated action
            EveryOnUpdated e <- atomically $ (view $ ix pid) <$> readTMVar everyOnUpdated
            -- Now execute any commands as a result of the state processing
            xs <- atomically $ runAction world frame (e *> o)
            runExec (exec xs) env
    renderCb <- liftIO $ J.syncCallback' doRender
    refCb <- liftIO $ J.syncCallback1 J.ContinueAsync doRef
    updatedCb <- liftIO $ J.syncCallback J.ContinueAsync doUpdated
    liftIO . atomically $ maybeModifyTMVar_ world $ \w -> do
        let ls = preview (myPlan._shimListeners) w
        case ls of
            -- plan doesn't exit
            Nothing -> pure Nothing
            -- shim listeners already created
            Just (Just _) -> pure Nothing
            Just Nothing -> pure . Just $ w & myPlan._shimListeners .~
                (Just $ ShimListeners renderCb updatedCb refCb)

execForkSTMAction :: forall c t m r.
    ( MonadIO m
    , MonadReader r m
    , HasItem' (TMVar (Scenario c t)) r
    , HasItem' (TVar (Scene t)) r
    )
    => (m () -> r -> IO ())
    -> (DL.DList c -> m ())
    -> ForkSTMAction c t
    -> m ()
execForkSTMAction runExec exec (ForkSTMAction m k) = do
    world <- view item' <$> ask
    frame <- view item' <$> ask
    env <- ask
    void . liftIO . forkIO $ do
        a <- atomically m
        xs <- atomically $ runAction world frame (k a)
        runExec (exec xs) env

#ifdef __GHCJS__

foreign import javascript unsafe
  "if ($2 && $2['setStates']) { $2['setStates']($1); }"
  js_setComponentStates :: JO.Object -> ComponentRef -> IO ()

#else

js_setComponentStates :: JO.Object -> ComponentRef -> IO ()
js_setComponentStates _ _ = pure ()

#endif
