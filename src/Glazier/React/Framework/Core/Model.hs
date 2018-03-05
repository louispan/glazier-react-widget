{-# LANGUAGE ConstraintKinds #-}
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
import Control.Monad.Reader
import qualified Data.DList as DL
import Data.Generics.Product
import Data.IORef
import qualified Data.Map.Strict as M
import qualified GHC.Generics as G
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import qualified Glazier.Core as Z
import qualified Glazier.React as Z

newtype GadgetId = GadgetId { unGadgetId :: J.JSString }
    deriving (G.Generic, Ord, Eq)

-- | One for every archetype, may be shared for many prototypes
data Plan m = Plan
    { component :: Z.ReactComponent
    , reactKey :: Z.ReactKey
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
    , listeners :: M.Map GadgetId (DL.DList Z.Listener)
    , refs :: M.Map GadgetId Z.EventTarget
    } deriving (G.Generic)

mkPlan :: Z.MonadReactor m => J.JSString -> m (Plan m)
mkPlan n = Plan
    <$> Z.doGetComponent
    <*> Z.doMkReactKey n
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
type Frame s m = (Plan m, s)

plan :: Lens' (Frame s m) (Plan m)
plan = _1

model :: Lens' (Frame s m) s
model = _2

-- | Mutable
type Scene v s m = Z.Obj IORef v (Frame s m)
type SceneDelegate v s m a = Z.Delegate (Scene v s m) m a
type MonadScene v s t m = (MonadReader (Scene v s m) (t m), MonadTrans t)

-- magnifyScene :: Lens' s' s -> (Scene v s m -> a) -> (Scene v s' m -> a)
-- magnifyScene l f = f . Z.edit (alongside id l)

editScene :: Lens' s' s -> (Scene v s' m -> Scene v s m)
editScene l = Z.edit (alongside id l)

-- Add an action to run once after the next render
addOnceOnUpdated :: (Z.MonadReactor m, MonadScene v s t m) => m () -> t m ()
addOnceOnUpdated k = do
    (Z.Obj ref its) <- ask
    lift $ Z.doModifyIORef' ref (its.plan.field @"onceOnUpdated" %~ (*> k))

-- Add an action to run after every render
addEveryOnUpdated :: (Z.MonadReactor m, MonadScene v s t m) =>  m () -> t m ()
addEveryOnUpdated k = do
    (Z.Obj ref its) <- ask
    lift $ Z.doModifyIORef' ref (its.plan.field @"everyOnUpdated" %~ (*> k))
