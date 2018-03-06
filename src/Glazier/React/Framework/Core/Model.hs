{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Glazier.React.Framework.Core.Model where

import qualified Control.Disposable as CD
import Control.Lens
import qualified Data.DList as DL
import Data.Generics.Product
import Data.IORef
import qualified Data.Map.Strict as M
import qualified GHC.Generics as G
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import Glazier.Core
import Glazier.React

newtype GadgetId = GadgetId { unGadgetId :: J.JSString }
    deriving (G.Generic, Ord, Eq)

-- | One for every archetype, may be shared for many prototypes
data Plan m = Plan
    { component :: ReactComponent
    , reactKey :: ReactKey
    , currentFrameNum :: Int
    , previousFrameNum :: Int
      -- things to dispose when this widget is removed
      -- cannot be hidden inside afterOnUpdated, as this needs to be used
      -- when finalizing
    , disposeOnRemoved :: CD.Disposable
    , disposeOnUpdated :: CD.Disposable -- ^ things to dispose on updated
    , everyOnUpdated :: m () -- ^ additional monadic action to take after every dirty
    , onceOnUpdated :: m () -- ^ additional monadic action to take after a dirty
    , onUpdated :: Maybe (J.Callback (J.JSVal -> IO ()))
    , onRender :: Maybe (J.Callback (IO J.JSVal))
    -- Storing listeners and refs in a 'M.Map', which simplifies the type of the model.
    -- Unfortunately, this means mismatches between event listeners and handlers
    -- are not compile time checked.
    -- Eg. `Glazier.React.Framework.Effect.MonadHTMLElement.focusRef` expects a ref
    -- but wont have compile error if `Glazier.React.Framework.Trigger.withRef` was not attached.
    -- The alternative is to store as a 'Many' in the model, but this ends up with messier types.
    , listeners :: M.Map GadgetId (DL.DList Listener)
    , refs :: M.Map GadgetId EventTarget
    } deriving (G.Generic)

_component :: Lens' (Plan m) ReactComponent
_component = field @"component"

_reactKey :: Lens' (Plan m) ReactKey
_reactKey = field @"reactKey"

_currentFrameNum :: Lens' (Plan m) Int
_currentFrameNum = field @"currentFrameNum"

_previousFrameNum :: Lens' (Plan m) Int
_previousFrameNum = field @"previousFrameNum"

_disposeOnRemoved :: Lens' (Plan m) CD.Disposable
_disposeOnRemoved = field @"disposeOnRemoved"

_disposeOnUpdated :: Lens' (Plan m) CD.Disposable
_disposeOnUpdated = field @"disposeOnUpdated"

_everyOnUpdated :: Lens' (Plan m) (m ())
_everyOnUpdated = field @"everyOnUpdated"

_onceOnUpdated :: Lens' (Plan m) (m ())
_onceOnUpdated = field @"onceOnUpdated"

_onUpdated :: Lens' (Plan m) (Maybe (J.Callback (J.JSVal -> IO ())))
_onUpdated = field @"onUpdated"

_onRender :: Lens' (Plan m) (Maybe (J.Callback (IO J.JSVal)))
_onRender = field @"onRender"

_listeners :: Lens' (Plan m) (M.Map GadgetId (DL.DList Listener))
_listeners = field @"listeners"

_refs :: Lens' (Plan m) (M.Map GadgetId EventTarget)
_refs = field @"refs"

mkPlan :: MonadReactor m => J.JSString -> m (Plan m)
mkPlan n = Plan
    <$> doGetComponent
    <*> doMkReactKey n
    <*> pure 0 -- ^ currentFrameNum
    <*> pure 0 -- ^ previousFrameNum
    <*> pure mempty -- ^ disposeOnRemoved
    <*> pure mempty -- ^ disposeOnUpdated
    <*> pure (pure ()) -- ^ everyOnUpdated
    <*> pure (pure ()) -- ^ onceOnUpdated
    <*> pure Nothing -- ^ callback
    <*> pure Nothing -- ^ render
    <*> pure M.empty
    <*> pure M.empty

-- Read-only
-- Using type synonym to a tuple for usages of 'alongside'.
-- type Frame m s = (Plan m, s)

-- plan :: Lens' (Frame m s) (Plan m)
-- plan = _1

-- model :: Lens' (Frame m s) s
-- model = _2

data Frame m s = Frame
    { plan :: Plan m
    , model :: s
    } deriving (G.Generic, Functor)

_plan :: Lens' (Frame m s) (Plan m)
_plan = field @"plan"

_model :: Lens' (Frame m s) s
_model = field @"model"

editFrame :: Lens' s' s -> Lens' (Frame m s') (Frame m s)
editFrame l = lens
    (\(Frame p s') -> Frame p (s' ^. l))
    (\(Frame _ s') (Frame p s) -> Frame p (s' & l .~ s))

-- | Mutable
type IOObj = Obj IORef
type Scene p m s = IOObj p (Frame m s)
-- type SceneDelegate p s m a = Delegate (Scene p m s) m a
-- type MonadScene p s t m = (MonadReader (Scene p m s) (t m), MonadTrans t)

-- magnifyScene :: Lens' s' s -> (Scene p m s -> a) -> (Scene p s' m -> a)
-- magnifyScene l f = f . edit (alongside id l)

editScene :: Lens' s' s -> Lens' (Scene p m s') (Scene p m s)
editScene l = accessor (editFrame l)

-- Add an action to run once after the next render
addOnceOnUpdated :: (MonadReactor m) => Scene p m s -> m () -> m ()
addOnceOnUpdated (Obj{..}) k = doModifyIORef' self (my._plan._onceOnUpdated %~ (*> k))

-- Add an action to run after every render
addEveryOnUpdated :: (MonadReactor m) => Scene p m s -> m () -> m ()
addEveryOnUpdated (Obj{..}) k = doModifyIORef' self (my._plan._everyOnUpdated %~ (*> k))
