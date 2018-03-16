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

import Control.Arrow
import Control.DeepSeq
import Control.Lens
import Control.Monad.Cont
import Control.Monad.Reader
import Control.Monad.RWS
import Control.Monad.State.Strict
import Data.Diverse.Lens
import qualified Data.DList as DL
import Data.Maybe
import Data.Semigroup
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import Glazier.Core
import Glazier.React
import Glazier.React.Framework.Core.Display
import Glazier.React.Framework.Core.Model
import qualified JavaScript.Extras as JE

------------------------------------------------------

-- -- | A simplified form of 'trigger' where all event info is dropped
-- -- and the given value is fired
-- trigger' :: (MonadReactor m)
--     => GadgetId
--     -> J.JSString
--     -> b
--     -> MethodT (Scene p m s) m b
-- trigger' gid n b = trigger gid n (const $ pure ()) (const b)

-- -- | Create callback for 'Notice' and add it to this state's dlist of listeners.
-- trigger ::
--     ( MonadReactor m
--     , NFData a
--     )
--     => GadgetId
--     -> J.JSString
--     -> (Notice -> IO a)
--     -> (a -> b)
--     -> MethodT (Scene p m s) m b
-- trigger gid n goStrict = mkListener gid n goStrict'
--   where
--     goStrict' e = case JE.fromJSR e of
--         Nothing -> pure Nothing
--         Just e' -> Just <$> goStrict e'


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

-- methodT' :: (r -> (a -> m ()) -> s -> m ()) -> MethodT r m a
-- methodT' = readrT' . (delegateT' .)

-- | Create callbacks and add it to this state's dlist of listeners.
-- NB. You probably want ot use 'trigger' instead since most React callbacks
-- generate a 'Notice'.
-- Only the "ref" callback generate 'EventTarget' in which case you would want
-- to use 'withRef' instead.
-- A 'Rerender' command for this widget is automatically added when  triggered.
mkListener ::
    ( NFData a
    , AsFacet (MkCallback1 a (StateT w m ())) x
    , AsFacet Rerender x
    , Monad m
    )
    => GadgetId
    -> J.JSString
    -> (JE.JSRep -> IO (Maybe a))
    -> (a -> StateT w m b)
    -> MethodT (ReifiedTraversal' w (Scene x s)) (StateT w m) b
mkListener gid n goStrict goLazy = do
    Traversal my <- ask
    lift $ delegateT' $ \fire -> do
        let goLazy' a = (goLazy a >>= fire) *> zoom my rerender
            msg = MkCallback1 goStrict goLazy' $ \cb ->
                my._plan._gadgets.at gid %= id
        zoom my (post1' msg)

    -- goLazy a >>= k
    -- -- The contination monad is used to allow do nation to compose the continuation
    -- -- to pass into MkCallback1.
    -- -- The final monad must be a "ContT m ()" so that it can betrivially run with 'pure'
    -- (`runContT` pure) $ do
    --     cb <- ContT $ post1' . MkCallback1 goStrict goLazy'
    --     let addListener = over _listeners (`DL.snoc` (n, cb))
    --     lift $ _plan._gadgets.at gid %= (Just . addListener . fromMaybe newGadget)

-- -- | This adds a ReactJS "ref" callback and MonadReactor effect to assign the ref into an EventTarget
-- -- in the plan
-- withRef ::
    --     ( MonadReactor m
    --     )
    --     => GadgetId
    --     -> MethodT (Scene p m s) m ()
    -- withRef i = mkListener i "ref" (pure . Just) id
    --     >>= hdlRef
    --   where
    --     -- hdlRef :: SceneHandler p s m (EventTarget) (Which '[])
    --     hdlRef j = readrT' $ \(Obj{..}) ->
--         lift $ doModifyIORef' self (my._plan._refs.at i .~ Just j
