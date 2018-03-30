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
data MkCallback1 c where
    MkCallback1 :: NFData a
        => TVar Plan
        -> TVar s
        -> (JE.JSRep -> IO (Maybe a))
        -> (a -> States (Scenario c s) ())
        -> (J.Callback (J.JSVal -> IO ()) -> States (Scenario c s) ())
        -> MkCallback1 c

data MkTick c where
    MkTick ::
        TVar Plan
        -> TVar s
        -> (States (Scenario c s) ())
        -> (IO () -> States (Scenario c s) ())
        -> MkTick c

-----------------------------------------------------------------

-- | Make the ShimListeners for this 'Plan' 'ShimListeners' using the given
-- 'Window' rendering function.
-- The original window should be dropped and the 'Widget' reduced to just a
-- 'Gadget' to emphasis the fact that the 'Window' was used.
data MkShimListeners where
    MkShimListeners ::
        Typeable s
        => TVar Plan
        -> TVar s
        -> (Window s ())
        -> MkShimListeners
