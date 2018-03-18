{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Glazier.React.Framework.Core.Reactor where

import Control.Applicative
import Control.Concurrent.MVar
import qualified Control.Disposable as CD
import Control.Lens
import Control.Monad.Morph
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import Data.Diverse.Lens
import qualified Data.DList as DL
import Data.Foldable
import Data.Maybe
import Data.Proxy
import Data.Tuple
import qualified GHCJS.Foreign.Callback as J
import Glazier.React
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


-- | env must contain empty MVar of state
runReactor ::
    ( MonadIO m
    , MonadReader r m
    , HasItem' (MVar (Scene x s)) r
    , HasItem' (Maybe (MVar QuitReactor)) r
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
    xs <- liftIO $ modifyMVar v $ \t -> swap <$> generalize (runStateT (ini *> takeCommands) t)
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
            QuitReactor <- liftIO $ takeMVar q
            -- get the disposables for the plan retrieve the final state
            (ds, s'') <- liftIO $ withMVar v $ \t -> fmap model <$> runStateT planDisposables t
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
    ( AsFacet QuitReactor x
    , AsFacet Rerender x
    , AsFacet (MkCallback1 (State (Scene x s) ())) x
    , HasItem' (Maybe (MVar QuitReactor)) r
    , HasItem' (MVar (Scene x s)) r
    , MonadReader r m
    , MonadIO m
    )
    => Proxy s -> (DL.DList x -> m ()) -> (m () -> r -> IO ()) -> x -> m ()
execReactor _ exec runExec x = fmap (fromMaybe mempty) $ runMaybeT $
    maybeExec execQuitReactor x
    <|> maybeExec execRerender x
    <|> maybeExec @(MkCallback1 (State (Scene x s) ())) (execMkCallback1 exec runExec) x

execQuitReactor ::
    ( HasItem' (Maybe (MVar QuitReactor)) r
    , MonadReader r m
    , MonadIO m
    )
    => QuitReactor -> m ()
execQuitReactor QuitReactor = do
    quit <- view item' <$> ask
    case quit of
        Nothing -> pure ()
        Just q -> liftIO $ void $ tryPutMVar q QuitReactor

execRerender ::
    ( MonadIO m
    )
    => Rerender -> m ()
execRerender (Rerender j i) = do
    liftIO $ js_setComponentState (JE.fromProperties [("frameNum", JE.toJSR i)]) j
    pure mempty

execMkCallback1 ::
    ( HasItem' (MVar (Scene x s)) r
    , MonadReader r m
    , MonadIO m
    )
    => (DL.DList x -> m ())
    -> (m () -> r -> IO ())
    -> MkCallback1 (State (Scene x s) ())
    -> m ()
execMkCallback1 exec runExec (MkCallback1 goStrict goLazy k) = do
    v <- view item' <$> ask
    env <- ask
    let f = handleEventM goStrict goLazy'
        goLazy' ma = case ma of
            -- trigger didn't produce anything useful
            Nothing -> pure mempty
            -- run state action using mvar
            Just a -> do
                xs <- modifyMVar v $ \t -> swap <$> generalize (runStateT (goLazy a *> takeCommands) t)
                -- Now execute any commands as a result of the state processing
                runExec (exec xs) env
    xs <- liftIO $ do
        cb <- J.syncCallback1 J.ContinueAsync (f . JE.JSRep)
        -- Now pass the cb into the continuation
        modifyMVar v $ \t -> swap <$> runStateT (hoist generalize (k cb) *> takeCommands) t
    -- Now execute any commands as a result of the adding the callback
    exec xs

-- doMkCallback goStrict goLazy = do
--     env <- ask
--     let goLazy' = (env &) . runReaderT . runIOReactor . goLazy
--     let f = Z.handleEventM goStrict goLazy'
--     liftIO $ do
--         cb <- J.syncCallback1 J.ContinueAsync (f . JE.JSRep)
--         let d = CD.dispose cb in pure (d, cb)


#ifdef __GHCJS__

foreign import javascript unsafe
  "if ($2 && $2['setState']) { $2['setState']($1); }"
  js_setComponentState :: JO.Object -> ComponentRef -> IO ()

#else

js_setComponentState :: JO.Object -> ComponentRef -> IO ()
js_setComponentState _ _ = pure ()

#endif
