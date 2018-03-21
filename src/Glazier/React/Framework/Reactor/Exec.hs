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
import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar.Extras
import qualified Control.Disposable as CD
import Control.Lens
import Control.Monad.Reader
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
import qualified GHCJS.Types as J
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
-- Then wait until @TMVar QuitReactor@ is not empty
-- before cleaning up callbacks stored in the @Plan@,
-- and then return with the latest state.
-- If @Maybe (TMVar QuitReactor)@ is Nothing
-- then runReactor will never cleanup, so after initalizing,
-- it will return the the initialized state.
-- It is expected that @m@ is a @MonadReader r m@
runReactor ::
    ( HasItem' (Maybe (TMVar QuitReactor)) env
    )
    => States (Scene x s) ()
    -> (TMVar (Scene x s) -> TVar (Scene x s) -> env -> r)
    -> (m () -> r -> IO ())
    -> (DL.DList x -> m ())
    -> env
    -> s
    -> IO s
runReactor ini addEnv runExec exec env s = do
    -- make state var
    let s' = Scene mempty newPlan s
    world <- newTMVarIO s'
    frame <- newTVarIO s'
    -- run through the app initialization
    xs <- atomically $ runFrame world frame ini
    -- now go through and evaluate the commands
    -- This will include making callbacks which will execute on other threads
    -- completely consume all commands
    runExec (exec xs) (addEnv world frame env)
    -- we have finished initializing, wait for the @quit@ command before cleanup
    let quit = view item' env
    case quit of
        -- The app never quits, so don't cleanup anything
        -- return the initialized state
        Nothing -> do
            s'' <- atomically $ model <$> readTMVar world
            pure s''
        -- This app quits, so we need to cleanup
        Just q -> do
            QuitReactor <- liftIO $ atomically $ takeTMVar q
            -- get the disposables for the plan retrieve the final state
            (ds, s'') <- liftIO $ atomically $ do
                t <- takeTMVar world
                fmap model <$> runStatesT planDisposables t
            -- Now cleanup the resources allocated on initialize
            -- This means JS listener callbacks will now result in exceptions
            -- so should only be done after the reactComponents have been removed
            -- from the DOM.
            liftIO $ fromMaybe (pure ()) (CD.runDisposable ds)
            pure s''
  where
    planDisposables :: Monad m => StatesT (Scene x s) m CD.Disposable
    planDisposables = CD.dispose <$> use _plan

runFrame :: TMVar (Scene x s) -> TVar (Scene x s) -> States (Scene x s) () -> STM (DL.DList x)
runFrame world frame action = do
    t <- takeTMVar world
    let (xs, t') = runStates (action *> takeCommands) t
    putTMVar world t'
    writeTVar frame t'
    pure xs
  where
    takeCommands :: States (Scene x s) (DL.DList x)
    takeCommands = do
        xs <- use _commands
        _commands .= mempty
        pure xs

-- Create a executor for all the core commands required by the framework
execReactor :: forall s x r m.
    ( MonadIO m
    , MonadReader r m
    , AsFacet (MkCallback1 (States (Scene x s) ())) x
    , AsFacet (MkEveryOnUpdatedCallback (States (Scene x s) ())) x
    , AsFacet (MkOnceOnUpdatedCallback (States (Scene x s) ())) x
    , AsFacet QuitReactor x
    , AsFacet Rerender x
    , HasItem' (TMVar (M.Map PlanId (EveryOnUpdated x s ()))) r
    , HasItem' (TMVar (M.Map PlanId (OnceOnUpdated x s ()))) r
    , HasItem' (Maybe (TMVar QuitReactor)) r
    , HasItem' (TMVar (Scene x s)) r
    , HasItem' (TVar (Scene x s)) r
    )
    => Proxy s -> (m () -> r -> IO ()) -> (DL.DList x -> m ()) -> x -> m ()
execReactor _ runExec exec x = fmap (fromMaybe mempty) $ runMaybeT $
    maybeExec @(MkCallback1 (States (Scene x s) ())) (execMkCallback1 runExec exec) x
    <|> maybeExec @(MkEveryOnUpdatedCallback (States (Scene x s) ())) execEveryOnUpdatedCallback x
    <|> maybeExec @(MkOnceOnUpdatedCallback (States (Scene x s) ())) execOnceOnUpdatedCallback x
    <|> maybeExec execQuitReactor x
    <|> maybeExec execRerender x

-- | An example of using the "tieing" 'execReactor' with itself. Lazy haskell is awesome.
-- NB. This tied executor *only* runs the Reactor effects.
reactorExecutor :: forall s x r m.
    ( MonadIO m
    , MonadReader r m
    , AsFacet (MkCallback1 (States (Scene x s) ())) x
    , AsFacet (MkEveryOnUpdatedCallback (States (Scene x s) ())) x
    , AsFacet (MkOnceOnUpdatedCallback (States (Scene x s) ())) x
    , AsFacet QuitReactor x
    , AsFacet Rerender x
    , HasItem' (TMVar (M.Map PlanId (EveryOnUpdated x s ()))) r
    , HasItem' (TMVar (M.Map PlanId (OnceOnUpdated x s ()))) r
    , HasItem' (Maybe (TMVar QuitReactor)) r
    , HasItem' (TMVar (Scene x s)) r
    , HasItem' (TVar (Scene x s)) r
    )
    => Proxy s -> (m () -> r -> IO ()) -> x -> m ()
reactorExecutor p runExec x = execReactor p runExec (traverse_ (reactorExecutor p runExec)) x

-----------------------------------------------------------------

execQuitReactor ::
    ( MonadIO m
    , MonadReader r m
    , HasItem' (Maybe (TMVar QuitReactor)) r
    )
    => QuitReactor -> m ()
execQuitReactor QuitReactor = do
    quit <- view item' <$> ask
    case quit of
        Nothing -> pure ()
        Just q -> liftIO $ atomically $ void $ tryPutTMVar q QuitReactor

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
    , HasItem' (TMVar (Scene x s)) r
    , HasItem' (TVar (Scene x s)) r
    )
    => (m () -> r -> IO ())
    -> (DL.DList x -> m ())
    -> MkCallback1 (States (Scene x s) ())
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
                xs <- atomically $ runFrame world frame (goLazy a)
                runExec (exec xs) env
    -- Apply to result to the world state, and execute any produced commands
    xs <- liftIO $ do
        cb <- J.syncCallback1 J.ContinueAsync (f . JE.JSRep)
        atomically $ runFrame world frame (k cb)
    exec xs

-----------------------------------------------------------------

newtype OnceOnUpdated x s b = OnceOnUpdated (States (Scene x s) b)
    deriving (G.Generic, Functor, Applicative, Monad, Semigroup, Monoid)

execOnceOnUpdatedCallback :: forall x s m r.
    ( MonadIO m
    , MonadReader r m
    , HasItem' (TMVar (M.Map PlanId (OnceOnUpdated x s ()))) r
    )
    => MkOnceOnUpdatedCallback (States (Scene x s) ())
    -> m ()
execOnceOnUpdatedCallback (MkOnceOnUpdatedCallback pid work) = do
    v <- view (item' @(TMVar (M.Map PlanId (OnceOnUpdated x s ())))) <$> ask
    liftIO . atomically . modifyTMVar_ v $ pure . over (at pid) (Just . (*> (OnceOnUpdated work)) . fromMaybe (pure ()))

-----------------------------------------------------------------

newtype EveryOnUpdated x s b = EveryOnUpdated (States (Scene x s) b)
    deriving (G.Generic, Functor, Applicative, Monad, Semigroup, Monoid)

execEveryOnUpdatedCallback :: forall x s m r.
    ( MonadIO m
    , MonadReader r m
    , HasItem' (TMVar (M.Map PlanId (EveryOnUpdated x s ()))) r
    )
    => MkEveryOnUpdatedCallback (States (Scene x s) ())
    -> m ()
execEveryOnUpdatedCallback (MkEveryOnUpdatedCallback pid work) = do
    v <- view (item' @(TMVar (M.Map PlanId (EveryOnUpdated x s ())))) <$> ask
    liftIO . atomically . modifyTMVar_ v $ pure . over (at pid) (Just . (*> (EveryOnUpdated work)) . fromMaybe (pure ()))

-----------------------------------------------------------------

-- | Making multiple MkShimListeners for the same plan is a silent error and will be ignored.
execMkShimListeners :: forall x s m r x' s'.
    ( MonadIO m
    , MonadReader r m
    , HasItem' (TMVar (Scene x s)) r
    , HasItem' (TVar (Scene x s)) r
    , HasItem' (TMVar (M.Map PlanId (OnceOnUpdated x s ()))) r
    , HasItem' (TMVar (M.Map PlanId (EveryOnUpdated x s ()))) r
    )
    => (m () -> r -> IO ())
    -> (DL.DList x -> m ())
    -> MkShimListeners (Scene x s) x' s'
    -> m ()
execMkShimListeners runExec exec (MkShimListeners pid (Traversal my) wind) = do
    world <- view item' <$> ask
    frame <- view item' <$> ask
    env <- ask
    onceOnUpdated <- view (item' @(TMVar (M.Map PlanId (OnceOnUpdated x s ())))) <$> ask
    everyOnUpdated <- view (item' @(TMVar (M.Map PlanId (EveryOnUpdated x s ())))) <$> ask
    let doRender = do
            frm <- atomically $ preview my <$> readTVar frame
            case frm of
                Nothing -> pure J.nullRef
                Just frm' -> let (as, _) = execRWSs wind frm' mempty
                            in JE.toJS <$> toElement as
        doRef j = atomically $ do
            let c = JE.fromJS j
            modifyTMVar_ world $ pure . (my._plan._componentRef .~ c)
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
            xs <- atomically $ runFrame world frame (e *> o)
            runExec (exec xs) env
    renderCb <- liftIO $ J.syncCallback' doRender
    refCb <- liftIO $ J.syncCallback1 J.ContinueAsync doRef
    updatedCb <- liftIO $ J.syncCallback J.ContinueAsync doUpdated
    liftIO . atomically $ maybeModifyTMVar_ world $ \w -> do
        let ls = preview (my._plan._shimListeners) w
        case ls of
            -- plan doesn't exit
            Nothing -> pure Nothing
            -- shim listeners already created
            Just (Just _) -> pure Nothing
            Just Nothing -> pure . Just $ w & my._plan._shimListeners .~
                (Just $ ShimListeners renderCb updatedCb refCb)

#ifdef __GHCJS__

foreign import javascript unsafe
  "if ($2 && $2['setStates']) { $2['setStates']($1); }"
  js_setComponentStates :: JO.Object -> ComponentRef -> IO ()

#else

js_setComponentStates :: JO.Object -> ComponentRef -> IO ()
js_setComponentStates _ _ = pure ()

#endif
