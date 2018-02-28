{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Glazier.React.Framework.Core.Model where

import qualified Control.Disposable as CD
import Control.Lens
import qualified Data.DList as DL
import Data.Generics.Product
import qualified Data.Map.Strict as M
import qualified GHC.Generics as G
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Core.Obj as R
import qualified JavaScript.Extras as JE

newtype GadgetId = GadgetId { unGadgetId :: J.JSString }
    deriving (G.Generic, Ord, Eq)

-- | One for every archetype, may be shared for many prototypes
data Plan m = Plan
    { component :: R.ReactComponent
    , reactKey :: R.ReactKey
    , currentFrameNum :: Int
    , previousFrameNum :: Int
      -- things to dispose when this widget is removed
      -- cannot be hidden inside afterOnUpdated, as this needs to be used
      -- when finalizing
    , disposeOnRemoved :: CD.Disposable
    , disposeOnUpdated :: CD.Disposable -- ^ things to dispose on updated
    , everyOnUpdated :: m () -- ^ additional monadic action to take after every stale
    , onceOnUpdated :: m () -- ^ additional monadic action to take after a stale
    , onUpdated :: Maybe (J.Callback (J.JSVal -> IO ()))
    , onRender :: Maybe (J.Callback (IO J.JSVal))
    -- Storing listeners and refs in a 'M.Map', which simplifies the type of the model.
    -- Unfortunately, this means mismatches between event listeners and handlers
    -- are not compile time checked.
    -- Eg. `Glazier.React.Framework.Effect.MonadHTMLElement.focusRef` expects a ref
    -- but wont have compile error if `Glazier.React.Framework.Trigger.withRef` was not attached.
    -- The alternative is to store as a 'Many' in the model, but this ends up with messier types.
    , listeners :: M.Map GadgetId (DL.DList R.Listener)
    , refs :: M.Map GadgetId R.EventTarget
    } deriving (G.Generic)

mkPlan :: R.MonadReactor m => J.JSString -> m (Plan m)
mkPlan n = Plan
    <$> R.doGetComponent
    <*> R.doMkReactKey n
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
type Frame m s = (Plan m, s)

plan :: Lens' (Frame m s) (Plan m)
plan = _1

model :: Lens' (Frame m s) s
model = _2

-- | Mutable
type Scene m v s = R.Obj v (Frame m s)

magnifyScene :: Lens' t s -> (Scene m v s -> a) -> (Scene m v t -> a)
magnifyScene l f = f . R.edit (alongside id l)

-- Add an action to run after a stale
addOnceOnUpdated :: R.MonadReactor m => Scene m v s -> m () -> m ()
addOnceOnUpdated (R.Obj ref its) k = R.doModifyIORef' ref (its.plan.field @"onceOnUpdated" %~ (*> k))

-- Add an action to run after a stale
addEveryOnUpdated :: R.MonadReactor m => Scene m v s -> m () -> m ()
addEveryOnUpdated (R.Obj ref its) k = R.doModifyIORef' ref (its.plan.field @"everyOnUpdated" %~ (*> k))

-- Marks the current widget as dirty, and stale required
stale :: R.MonadReactor m => Scene m v s -> m ()
stale (R.Obj ref its) =
    R.doModifyIORef' ref (its.plan.field @"currentFrameNum" %~ ((+ 1) . (`mod` JE.maxSafeInteger)))
