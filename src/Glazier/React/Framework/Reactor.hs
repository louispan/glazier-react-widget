{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Glazier.React.Framework.Reactor where

import Control.Concurrent.STM
import Control.DeepSeq
import Control.Lens
import Control.Monad.State
import Control.Monad.Trans.States.Strict
import Data.Diverse.Lens
import Data.Typeable
import Glazier.React
import Glazier.React.Framework.Scene
import Glazier.React.Framework.Window
import qualified JavaScript.Extras as JE

-----------------------------------------------------------------

data Rerender where
    Rerender :: Typeable p
        => ComponentRef
        -> p
        -> Rerender

-- Marks the current widget as dirty, and rerender is required
-- A 'rerender' will called at the very end of a 'Glazier.React.Framework.Trigger.trigger'
-- This means calling 'dirty' on other widgets from a different widget's 'Glazier.React.Framework.Trigger.trigger'
-- will not result in a rerender for the other widget.
dirty :: (MonadState (Scenario c s) m) => m ()
dirty = _scene._plan._currentFrameNum %= JE.safeModularIncrement

rerender ::
    ( Typeable s
    , AsFacet Rerender c
    , MonadState (Scenario c s) m
    )
    => m ()
rerender = do
    -- check frame num to see if dirty has been called
    c <- use (_scene._plan._currentFrameNum)
    p <- use (_scene._plan._previousFrameNum)
    comp <- use (_scene._plan._componentRef)
    -- we are dirty
    case (c /= p, comp) of
        (True, Just comp') -> do
            _scene._plan._previousFrameNum .= c
            scn <- use _scene
            post1 $ Rerender comp' scn
        _ -> pure ()

-----------------------------------------------------------------
data TickState c where
    TickState ::
        TVar Plan
        -> TVar s
        -> (States (Scenario c s) ())
        -> TickState c

data MkAction1 c where
    MkAction1 :: NFData a
        => (JE.JSRep -> IO (Maybe a))
        -> (a -> c)
        -> ((JE.JSRep -> IO ()) -> c)
        -> MkAction1 c

-- | Convert a command to an IO action
data MkAction c where
    MkAction ::
        c
        -> (IO () -> c)
        -> MkAction c

data MkTick1 c where
    MkTick1 :: NFData a
        => TVar Plan
        -> TVar s
        -> (JE.JSRep -> IO (Maybe a))
        -> (a -> States (Scenario c s) ())
        -> ((JE.JSRep -> IO ()) -> States (Scenario c s) ())
        -> MkTick1 c

data MkTick c where
    MkTick ::
        TVar Plan
        -> TVar s
        -> (States (Scenario c s) ())
        -> (IO () -> States (Scenario c s) ())
        -> MkTick c

-----------------------------------------------------------------

-- | Make the 'ShimCallbacks' for this 'Plan' using the given
-- 'Window' rendering function.
-- The original window should be dropped and the 'Widget' reduced to just a
-- 'Gadget' to emphasis the fact that the 'Window' was used.
data MkShimCallbacks where
    MkShimCallbacks ::
        Typeable s
        => TVar Plan
        -> TVar s
        -> (Window s ())
        -> MkShimCallbacks


data ForkSTM c where
    ForkSTM ::
        -- blockable STM to fork
        STM a
        -- Continuation to run when STM succeeds.
        -> (a -> c)
        -> ForkSTM c
