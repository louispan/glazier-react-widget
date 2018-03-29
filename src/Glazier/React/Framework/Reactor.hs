{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Glazier.React.Framework.Reactor where

import Control.Concurrent.STM
import Control.DeepSeq
import qualified Control.Disposable as CD
import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.States.Strict
import Data.Diverse.Lens
import Data.Typeable
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Foreign.Export as J
import qualified GHCJS.Types as J
import Glazier.React
import Glazier.React.Framework.Gadget
import Glazier.React.Framework.MkId
import Glazier.React.Framework.Scene
import Glazier.React.Framework.Window
import qualified JavaScript.Extras as JE

-----------------------------------------------------------------

-- data RunDisposable = RunDisposable CD.Disposable

data Rerender where
    Rerender :: Typeable p
        => ComponentRef
        -> p
        -> Rerender

-- Marks the current widget as dirty, and rerender is required
-- A 'rerender' will called at the very end of a 'Glazier.React.Framework.Trigger.trigger'
-- This means calling 'dirty' on other widgets from a different widget's 'Glazier.React.Framework.Trigger.trigger'
-- will not result in a rerender for the other widget.
dirty :: (HasPlan s, MonadState s m) => m ()
dirty = _plan._currentFrameNum %= JE.safeModularIncrement

rerender ::
    ( Typeable s
    , AsFacet Rerender c
    , HasPlan (p s)
    , HasScene p
    , HasCommands c (p s)
    , MonadState (p s) m
    )
    => m ()
rerender = do
    -- check frame num to see if dirty has been called
    c <- use (_plan._currentFrameNum)
    p <- use (_plan._previousFrameNum)
    comp <- use (_plan._componentRef)
    -- we are dirty
    case (c /= p, comp) of
        (True, Just comp') -> do
            _plan._previousFrameNum .= c
            scn <- use _scene
            post1 $ Rerender comp' scn
        _ -> pure ()

-----------------------------------------------------------------

-- | Creates a callback that is injected into the engine processing:
-- * DOM listener is generated
-- * The strict part of the callback is invoked
-- * If the result is nothing, finish
-- * Else, world state ('Scene' without 'commands', ie treating commands like Writer)
--  is retrieved from a ref
-- * The provided traversal is used to zoom the state
-- * If the state is Nothing, finish
-- * Else, the lazy part of the callback (ie the bind function for the state monad)
-- is invoked with the result from the strict callback,
-- using the state retrieved with 'commands' set to memtpy.
-- * The state ref is updated with the changed state.
-- * The 'commands' of the state is collected.
-- * Each of the 'commands' is processed, which may further modify the state.
--
-- After creating the above callback, re-run the monad
-- (getting the state from the ref, etc)
-- using the last continuation arg
-- THen save the state back into the ref + command processing
data MkCallback1 c p where
    MkCallback1 :: NFData a
        => (JE.JSRep -> IO (Maybe a))
        -> (a -> States (Scenario c p) ())
        -> (J.Callback (J.JSVal -> IO ()) -> States (Scenario c p) ())
        -> MkCallback1 c p

-----------------------------------------------------------------

-- | The engine should use the given id and add the stateful action @next@
-- to the onUpdated callback of the react component.
-- That is the engine should store a map of state actions for this react component in a var.
-- Then the onUpdated callback of a react component can:
-- read the var to get the currently stored state action for this react component
-- read the current state from the var
-- apply the stored state actions to the state
-- clear the stored state actions
-- save the new state
-- process any commands generated.
data MkOnceOnUpdatedCallback c t =
    MkOnceOnUpdatedCallback PlanId (States (Scenario c t) ())

-----------------------------------------------------------------

-- | Similar to 'MkOnceOnUpdatedCallback' except the state action
-- gets added to a separate map which doesn't get cleared.
data MkEveryOnUpdatedCallback c t =
    MkEveryOnUpdatedCallback PlanId (States (Scenario c t) ())

-- | Make the ShimListeners for this 'Plan' 'ShimListeners' using the given
-- 'Window' rendering function.
-- The original window should be dropped and the 'Widget' reduced to just a
-- 'Gadget' to emphasis the fact that the 'Window' was used.
data MkShimListeners c t = MkShimListeners
    PlanId
    (ReifiedTraversal' (Scenario c t) Plan)
    (Window t ())

-- | Spawn a thread to run the stm until it succeeds
data ForkSTMAction c t where
    ForkSTMAction :: STM a -> (a -> States (Scenario c t) ()) -> ForkSTMAction c t
