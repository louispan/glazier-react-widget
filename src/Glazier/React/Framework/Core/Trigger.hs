{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Framework.Core.Trigger where

import Control.DeepSeq
import Control.Lens
import Control.Monad.Cont
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Delegate
import Data.Diverse.Lens
import qualified Data.DList as DL
import Data.Maybe
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import Glazier.React
import Glazier.React.Framework.Core.Gadget
import Glazier.React.Framework.Core.MkId
import Glazier.React.Framework.Core.Model
import Glazier.React.Framework.Core.Window
import qualified JavaScript.Extras as JE

------------------------------------------------------

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
data MkCallback1 next where
    MkCallback1 :: NFData a
        => (JE.JSRep -> IO (Maybe a))
        -> (a -> next)
        -> (J.Callback (J.JSVal -> IO ()) -> next)
        -> MkCallback1 next

instance Functor MkCallback1 where
    fmap f (MkCallback1 goStrict goLazy g) = MkCallback1 goStrict (f . goLazy) (f . g)

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
data MkOnceOnUpdatedCallback next =
    MkOnceOnUpdatedCallback PlanId next
        deriving Functor

-- | Similar to 'MkOnceOnUpdatedCallback' except the state action
-- gets added to a separate map which doesn't get cleared.
data MkEveryOnUpdatedCallback next =
    MkEveryOnUpdatedCallback PlanId next
        deriving Functor

-- | Create a callback and add it to this gizmo's dlist of listeners.
-- NB. You probably want to use 'trigger' instead since most React callbacks
-- generate a 'Notice'.
-- Only the "ref" callback generate 'EventTarget' or 'ComponentRef' in which case you would want
-- to use 'withRef' instead.
mkListener ::
    ( NFData a
    , AsFacet (MkCallback1 (StateT w m ())) x
    , Monad m
    )
    => GizmoId
    -> J.JSString
    -> (JE.JSRep -> IO (Maybe a))
    -> (a -> StateT w m b)
    -> StateT w m ()
    -> GadgetT w x s m b
mkListener gid n goStrict goLazy extra = do
    Traversal my <- ask
    lift $ delegateT' $ \fire -> do
        let goLazy' a = (goLazy a >>= fire) *> extra
            msg = MkCallback1 goStrict goLazy' $ \cb -> do
                let addListener = over _listeners (`DL.snoc` (n, cb))
                my._plan._gizmos.at gid %= (Just . addListener . fromMaybe newGizmo)
        zoom my (post1' msg)

-- | A 'trigger' where all event info is dropped and the given value is fired.
trigger' ::
    ( AsFacet (MkCallback1 (StateT w m ())) x
    , AsFacet Rerender x
    , Monad m
    )
    => GizmoId
    -> J.JSString
    -> b
    -> GadgetT w x s m b
trigger' gid n b = do
    Traversal my <- ask
    mkListener gid n (const $ pure (Just ())) (const $ pure b) (zoom my rerender)

-- | Create callback for 'Notice' and add it to this gizmos's dlist of listeners.
-- Also adds a 'Rerender' command at the end of the callback
trigger ::
    ( NFData a
    , AsFacet (MkCallback1 (StateT w m ())) x
    , AsFacet Rerender x
    , Monad m
    )
    => GizmoId
    -> J.JSString
    -> (Notice -> IO a)
    -> (a -> StateT w m b)
    -> GadgetT w x s m b
trigger gid n goStrict goLazy = do
    Traversal my <- ask
    mkListener gid n goStrict' goLazy (zoom my rerender)
  where
    goStrict' e = case JE.fromJSR e of
        Nothing -> pure Nothing
        Just e' -> Just <$> goStrict e'

-- | This adds a ReactJS "ref" callback assign the ref into an EventTarget for the
-- gizmo in the plan
withRef ::
        ( AsFacet (MkCallback1 (StateT w m ())) x
        , Monad m
        )
        => GizmoId
        -> GadgetT w x s m ()
withRef gid = do
    Traversal my <- ask
    mkListener gid "ref" (pure . Just) (hdlRef my) (pure ())
  where
    hdlRef my j = let evt = JE.fromJSR j
               in my._plan._gizmos.ix gid._targetRef .= evt

