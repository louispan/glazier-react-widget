{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Glazier.React.Framework.Reactor where

import Control.Concurrent.STM
import Control.DeepSeq
import qualified Control.Disposable as CD
import Control.Lens
import Control.Monad.State
import Control.Monad.Trans.States.Strict
import Data.Diverse.Lens
import Data.Typeable
import Glazier.React
import Glazier.React.Framework.Scene
import Glazier.React.Framework.Window
import qualified JavaScript.Extras as JE

-- | convert a request type to a command type.
-- This is used for commands that doesn't have a continuation.
-- Ie. commands that doesn't "returns" a value from running an effect.
-- Use 'cmd'' for commands that require a continuation ("returns" a value).
cmd :: (AsFacet c' c) => c' -> c
cmd = review facet

-- | A variation of 'cmd' for commands with a type variable @c@,
-- which is usually commands that are containers of command,
-- or commands that require a continuation
-- Eg. commands that "returns" a value from running an effect.
-- 'cmd'' is usually used with with the 'Cont' monad to help
-- create the continuation.
--
-- @
-- post $ (`runCont` id) $ do
--     a <- cont $ cmd' . GetSomething
--     pure . cmd $ DoSomething (f a)
-- @
cmd' :: (AsFacet (c' c) c) => c' c -> c
cmd' = cmd

memptyCmd :: AsFacet [c] c => c
memptyCmd = cmd' @[] []

cmds :: AsFacet [c] c => [c] -> c
cmds = cmd' @[]

-----------------------------------------------------------------

type ReactorCmds c =
    '[ [c]
    , Rerender
    , TickState c
    , MkAction1 c
    , MkAction c
    , MkShimCallbacks
    , CD.Disposable
    , ForkSTM c
    ]

type AsReactor c =
    ( AsFacet [c] c
    , AsFacet Rerender c
    , AsFacet (TickState c) c
    , AsFacet (MkAction1 c) c
    , AsFacet (MkAction c) c
    , AsFacet MkShimCallbacks c
    , AsFacet CD.Disposable c
    , AsFacet (ForkSTM c) c
    )

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
            post . cmd $ Rerender comp' scn
        _ -> pure ()

-- The executor of this command must automatically check 'rerender' at the end of this state tick
data TickState c where
    TickState ::
        Typeable s
        => TVar Plan
        -> TVar s
        -> (States (Scenario c s) ())
        -> TickState c

-- | Convert a command to an IO action
data MkAction c where
    MkAction ::
        c
        -> (IO () -> c)
        -> MkAction c

-- | Convert a callback to a @JE.JSRep -> IO ()@
data MkAction1 c where
    MkAction1 :: NFData a
        => (JE.JSRep -> IO (Maybe a))
        -> (a -> c)
        -> ((JE.JSRep -> IO ()) -> c)
        -> MkAction1 c

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

-- | Runs a blockable STM.
-- The executor should never be blocked from executing the next command.
-- Ie. the executor should always execute this STM in a concurrent thread just in case the STM blocks on
-- commands *after* this 'RunSTM'.
-- Eg. the STM reads from a channel will will be unblocked by a later STM.
data ForkSTM c where
    ForkSTM ::
        -- blockable STM to run
        STM a
        -- Continuation to run when STM succeeds.
        -> (a -> c)
        -> ForkSTM c
