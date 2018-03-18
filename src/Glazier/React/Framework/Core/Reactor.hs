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

module Glazier.React.Framework.Core.Reactor where

import Control.Applicative
import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar.Extras
import qualified Control.Disposable as CD
import Control.Lens
import Control.Monad.Morph
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import Data.Diverse.Lens
import qualified Data.DList as DL
import Data.Foldable
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Proxy
import Data.Tuple
import qualified GHC.Generics as G
import qualified GHCJS.Foreign.Callback as J
import Glazier.React
import Glazier.React.Framework.Core.MkId
import Glazier.React.Framework.Core.Model
import Glazier.React.Framework.Core.Trigger
import Glazier.React.Framework.Core.Window
import qualified JavaScript.Extras as JE
import qualified JavaScript.Object as JO

data QuitReactor = QuitReactor

    -- -- create an initial scene for the model
    -- let s' = Scene mempty (newPlan pid) s
    -- -- save the state in a MVar to block other threads from callbacks
    -- -- So that only one state modifying action is used at a time.
    -- v <- newMVar s'
    -- -- a signal to quit
    -- quit <- newEmptyMVar

-- -- | A 'World' contains additional interactively data not directly exposed in Scene.
-- data World x s =  World
--     { everyOnUpdated :: M.Map PlanId
--     , scene :: Scene x s
--     } deriving (G.Generic)

-- | env must contain empty MVar of state
runReactor ::
    ( MonadIO m
    , MonadReader r m
    , HasItem' (TMVar (Scene x s)) r
    , HasItem' (Maybe (TMVar QuitReactor)) r
    )
    => State (Scene x s) ()
    -> (DL.DList x -> m ())
    -> s
    -> m s
runReactor ini exec s = do
    -- get state var
    v <- view item' <$> ask
    -- run through the app initialization
    -- let init = runGadgetT appGadget id (const $ pure ())
    xs <- liftIO $ atomically $ modifyTMVar v $ \t -> swap <$> generalize (runStateT (ini *> takeCommands) t)
    -- now go through and evaluate the commands
    -- This will include making callbacks which will execute on other threads
    -- completely consume all commands
    exec xs
    -- we have finished initializing, wait for the @quit@ command before cleanup
    quit <- view item' <$> ask
    case quit of
        -- The app never quits, so don't cleanup anything
        -- pretend to return the initialState
        Nothing -> pure s
        -- This app quits, so we need to cleanup
        Just q -> do
            QuitReactor <- liftIO $ atomically $ takeTMVar q
            -- get the disposables for the plan retrieve the final state
            (ds, s'') <- liftIO $ atomically $ takeTMVar v >>= \t -> fmap model <$> runStateT planDisposables t
            -- Now cleanup the resources allocated on initialize
            -- This means JS callbacks will now result in exceptions
            liftIO $ fromMaybe (pure ()) (CD.runDisposable ds)
            pure s''

  where
    planDisposables :: Monad m => StateT (Scene x s) m CD.Disposable
    planDisposables = CD.dispose <$> use _plan

takeCommands :: Monad m => StateT (Scene x s) m (DL.DList x)
takeCommands = do
    xs <- use _commands
    _commands .= mempty
    pure xs

execListReactor :: Applicative m => (x -> m ()) -> DL.DList x -> m ()
execListReactor = traverse_

maybeExec :: forall a x m b. (Monad m, AsFacet a x) => (a -> m b) -> x -> MaybeT m b
maybeExec k y = maybe empty pure (preview facet y) >>= (lift <$> k)

-- | Examples
execReactor :: forall s x r m.
    ( MonadReader r m
    , MonadIO m
    , AsFacet (MkCallback1 (State (Scene x s) ())) x
    , AsFacet (MkEveryOnUpdatedCallback (State (Scene x s) ())) x
    , AsFacet (MkOnceOnUpdatedCallback (State (Scene x s) ())) x
    , AsFacet QuitReactor x
    , AsFacet Rerender x
    , HasItem' (TMVar (M.Map PlanId (EveryOnUpdated x s ()))) r
    , HasItem' (TMVar (M.Map PlanId (OnceOnUpdated x s ()))) r
    , HasItem' (Maybe (TMVar QuitReactor)) r
    , HasItem' (TMVar (Scene x s)) r
    )
    => Proxy s -> (m () -> r -> IO ()) -> (DL.DList x -> m ()) -> x -> m ()
execReactor _ runExec exec x = fmap (fromMaybe mempty) $ runMaybeT $
    maybeExec @(MkCallback1 (State (Scene x s) ())) (execMkCallback1 runExec exec) x
    <|> maybeExec @(MkEveryOnUpdatedCallback (State (Scene x s) ())) execEveryOnUpdatedCallback x
    <|> maybeExec @(MkOnceOnUpdatedCallback (State (Scene x s) ())) execOnceOnUpdatedCallback x
    <|> maybeExec execQuitReactor x
    <|> maybeExec execRerender x

-- | An example of using the execReactor itself to pass into @execMkCallback1@
-- lazy haskell is awesome.
execReactor' :: forall s x r m.
    ( MonadReader r m
    , MonadIO m
    , AsFacet (MkCallback1 (State (Scene x s) ())) x
    , AsFacet (MkEveryOnUpdatedCallback (State (Scene x s) ())) x
    , AsFacet (MkOnceOnUpdatedCallback (State (Scene x s) ())) x
    , AsFacet QuitReactor x
    , AsFacet Rerender x
    , HasItem' (TMVar (M.Map PlanId (EveryOnUpdated x s ()))) r
    , HasItem' (TMVar (M.Map PlanId (OnceOnUpdated x s ()))) r
    , HasItem' (Maybe (TMVar QuitReactor)) r
    , HasItem' (TMVar (Scene x s)) r
    )
    => Proxy s -> (m () -> r -> IO ()) -> x -> m ()
execReactor' p runExec x = execReactor p runExec (traverse_ (execReactor' p runExec)) x

execQuitReactor ::
    ( MonadReader r m
    , MonadIO m
    , HasItem' (Maybe (TMVar QuitReactor)) r
    )
    => QuitReactor -> m ()
execQuitReactor QuitReactor = do
    quit <- view item' <$> ask
    case quit of
        Nothing -> pure ()
        Just q -> liftIO $ atomically $ void $ tryPutTMVar q QuitReactor

execRerender ::
    ( MonadIO m
    )
    => Rerender -> m ()
execRerender (Rerender j i) = do
    liftIO $ js_setComponentState (JE.fromProperties [("frameNum", JE.toJSR i)]) j
    pure mempty

execMkCallback1 ::
    ( MonadReader r m
    , MonadIO m
    , HasItem' (TMVar (Scene x s)) r
    )
    => (m () -> r -> IO ())
    -> (DL.DList x -> m ())
    -> MkCallback1 (State (Scene x s) ())
    -> m ()
execMkCallback1 runExec exec (MkCallback1 goStrict goLazy k) = do
    v <- view item' <$> ask
    env <- ask
    let f = handleEventM goStrict goLazy'
        goLazy' ma = case ma of
            -- trigger didn't produce anything useful
            Nothing -> pure mempty
            -- run state action using mvar
            Just a -> do
                xs <- atomically $ modifyTMVar v $ \t -> swap <$> generalize (runStateT (goLazy a *> takeCommands) t)
                -- Now execute any commands as a result of the state processing
                runExec (exec xs) env
    xs <- liftIO $ do
        cb <- J.syncCallback1 J.ContinueAsync (f . JE.JSRep)
        -- Now pass the cb into the continuation
        atomically $ modifyTMVar v $ \t -> swap <$> runStateT (hoist generalize (k cb) *> takeCommands) t
    -- Now execute any commands as a result of the adding the callback
    exec xs

newtype EveryOnUpdated x s b = EveryOnUpdated (State (Scene x s) b)
    deriving (G.Generic, Functor, Applicative, Monad)

execEveryOnUpdatedCallback :: forall x s m r.
    ( MonadReader r m
    , MonadIO m
    , HasItem' (TMVar (M.Map PlanId (EveryOnUpdated x s ()))) r
    )
    => MkEveryOnUpdatedCallback (State (Scene x s) ())
    -> m ()
execEveryOnUpdatedCallback (MkEveryOnUpdatedCallback pid work) = do
    v <- view (item' @(TMVar (M.Map PlanId (EveryOnUpdated x s ())))) <$> ask
    liftIO . atomically . modifyTMVar_ v $ pure . over (at pid) (Just . (*> (EveryOnUpdated work)) . fromMaybe (pure ()))

newtype OnceOnUpdated x s b = OnceOnUpdated (State (Scene x s) b)
    deriving (G.Generic, Functor, Applicative, Monad)

execOnceOnUpdatedCallback :: forall x s m r.
    ( MonadReader r m
    , MonadIO m
    , HasItem' (TMVar (M.Map PlanId (OnceOnUpdated x s ()))) r
    )
    => MkOnceOnUpdatedCallback (State (Scene x s) ())
    -> m ()
execOnceOnUpdatedCallback (MkOnceOnUpdatedCallback pid work) = do
    v <- view (item' @(TMVar (M.Map PlanId (OnceOnUpdated x s ())))) <$> ask
    liftIO . atomically . modifyTMVar_ v $ pure . over (at pid) (Just . (*> (OnceOnUpdated work)) . fromMaybe (pure ()))

#ifdef __GHCJS__

foreign import javascript unsafe
  "if ($2 && $2['setState']) { $2['setState']($1); }"
  js_setComponentState :: JO.Object -> ComponentRef -> IO ()

#else

js_setComponentState :: JO.Object -> ComponentRef -> IO ()
js_setComponentState _ _ = pure ()

#endif
