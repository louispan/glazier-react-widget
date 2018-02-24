{-# LANGUAGE AllowAmbiguousTypes #-}
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
import qualified Data.Map.Strict as M
import qualified GHC.Generics as G
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Core.Obj as R

newtype GadgetId = GadgetId { unGadgetId :: J.JSString }
    deriving (G.Generic, Ord, Eq)

-- | One for every archetype, may be shared for many prototypes
data Plan m = Plan
    { component :: R.ReactComponent
    , reactKey :: R.ReactKey
    , frameNum :: Int
    , -- things to dispose when this widget is removed
      -- cannot be hidden inside afterOnUpdated, as this needs to be used
      -- when finalizing
      disposeOnRemoved :: CD.Disposable
    , disposeOnUpdated :: CD.Disposable -- ^ things to dispose on updated
    , everyOnUpdated :: m () -- ^ additional monadic action to take after every rerender
    , onceOnUpdated :: m () -- ^ additional monadic action to take after a rerender
    , onUpdated :: Maybe (J.Callback (J.JSVal -> IO ()))
    , onRender :: Maybe (J.Callback (IO J.JSVal))
    -- Storing listeners and refs in a 'M.Map', which simplifies the type of the model.
    -- Unfortunately, this means mismatches between event listeners and handlers
    -- are not compile time checked.
    -- Eg. `Glazier.React.Framework.Effect.MonadHTMLElement.focusRef` expects a ref
    -- but wont have compile error if `Glazier.React.Framework.TriggeR.withRef` was not attached.
    -- The alternative is to store as a 'Many' in the model, but this ends up with messier types.
    , listeners :: M.Map GadgetId [R.Listener]
    , refs :: M.Map GadgetId R.EventTarget
    } deriving (G.Generic)

mkPlan :: R.MonadReactor m => J.JSString -> m (Plan m)
mkPlan n = Plan
    <$> R.doGetComponent
    <*> R.doMkReactKey n
    <*> pure 0 -- ^ frameNum
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
