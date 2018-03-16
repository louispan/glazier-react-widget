{-# LANGUAGE DataKinds #-}
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
import Glazier.React.Framework.Core.Display
import Glazier.React.Framework.Core.Method
import Glazier.React.Framework.Core.Model
import qualified JavaScript.Extras as JE

------------------------------------------------------

-- Creates a callback that is injected into the engine processing:
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
--
-- ?? Does providing a Traversal lens really work?
-- - need to test
-- ?? How to zoom the Plan? Same thing, Reader of traversal to current plan
data MkCallback1 a next where
    MkCallback1 :: NFData a
        => (JE.JSRep -> IO (Maybe a))
        -> (a -> next)
        -> (J.Callback (J.JSVal -> IO ()) -> next)
        -> MkCallback1 a next

instance Functor (MkCallback1 a) where
    fmap f (MkCallback1 goStrict goLazy g) = MkCallback1 goStrict (f . goLazy) (f . g)

-- | Create a callback and add it to this state's dlist of listeners.
-- NB. You probably want to use 'trigger' instead since most React callbacks
-- generate a 'Notice'.
-- Only the "ref" callback generate 'EventTarget' or 'ComponentRef' in which case you would want
-- to use 'withRef' instead.
mkListener ::
    ( NFData a
    , AsFacet (MkCallback1 a (StateT w m ())) x
    , Monad m
    )
    => GadgetId
    -> J.JSString
    -> (JE.JSRep -> IO (Maybe a))
    -> (a -> StateT w m b)
    -> StateT w m ()
    -> MethodT w x s m b
mkListener gid n goStrict goLazy extra = do
    Traversal my <- ask
    lift $ delegateT' $ \fire -> do
        let goLazy' a = (goLazy a >>= fire) *> extra
            msg = MkCallback1 goStrict goLazy' $ \cb -> do
                let addListener = over _listeners (`DL.snoc` (n, cb))
                my._plan._gadgets.at gid %= (Just . addListener . fromMaybe newGadget)
        zoom my (post1' msg)

-- | A 'trigger' where all event info is dropped and the given value is fired.
trigger' ::
    ( AsFacet (MkCallback1 () (StateT w m ())) x
    , AsFacet Rerender x
    , Monad m
    )
    => GadgetId
    -> J.JSString
    -> b
    -> MethodT w x s m b
trigger' gid n b = do
    Traversal my <- ask
    mkListener gid n (const $ pure (Just ())) (const $ pure b) (zoom my rerender)

-- | Create callback for 'Notice' and add it to this state's dlist of listeners.
-- Also adds a 'Rerender' command at the end of the callback
trigger ::
    ( NFData a
    , AsFacet (MkCallback1 a (StateT w m ())) x
    , AsFacet Rerender x
    , Monad m
    )
    => GadgetId
    -> J.JSString
    -> (Notice -> IO a)
    -> (a -> StateT w m b)
    -> MethodT w x s m b
trigger gid n goStrict goLazy = do
    Traversal my <- ask
    mkListener gid n goStrict' goLazy (zoom my rerender)
  where
    goStrict' e = case JE.fromJSR e of
        Nothing -> pure Nothing
        Just e' -> Just <$> goStrict e'

-- | This adds a ReactJS "ref" callback assign the ref into an EventTarget in the plan
withRef ::
        ( AsFacet (MkCallback1 JE.JSRep (StateT w m ())) x
        , Monad m
        )
        => GadgetId
        -> MethodT w x s m ()
withRef gid = do
    Traversal my <- ask
    mkListener gid "ref" (pure . Just) (hdlRef my) (pure ())
  where
    hdlRef my j = let evt = JE.fromJSR j
               in my._plan._gadgets.ix gid._targetRef .= evt
