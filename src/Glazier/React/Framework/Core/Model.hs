{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Framework.Core.Model where

import qualified Control.Disposable as CD
import Control.Lens
import Data.Functor.Contravariant
import Data.Kind
import qualified GHC.Generics as G
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Core.Obj as F

----------------------------------------------------------

type family OnSpec (w :: Type -> Type) (s :: Type) = (r :: Type) | r -> w s

-- | Something that knows how to get and set (but not make) a spec
class ViaSpec (w :: Type -> Type) where
    -- | given a lens from @t@ to @s@,
    -- change something that knows how to manipulate an @s@
    -- to something that knows how to manipulate a @t@.
    viaSpec :: Lens' t s -> OnSpec w s -> OnSpec w t

type instance OnSpec (Op a) s = s -> a

instance ViaSpec (Op a) where
    viaSpec l f = f . view l

type family OnInfo (w :: Type -> Type) (i :: Type) = (r :: Type) | r -> w i

-- | Something that knows how to get and set (but not make) a plan
class ViaInfo (w :: Type -> Type) where
    -- | given lens from @q@ to @p@,
    -- change something that knows how to manipulate an @p@
    -- to something that knows how to manipulate a @q@.
    viaInfo :: Lens' j i -> OnInfo w i -> OnInfo w j

type instance OnInfo (Op a) s = s -> a

instance ViaInfo (Op a) where
    viaInfo l f = f . view l

-- | One for every archetype, may be shared for many prototypes
data Plan m = Plan
    { component :: R.ReactComponent
    , reactKey :: R.ReactKey
    , frameNum :: Int
    , disposeOnRemoved :: CD.Disposable -- things to dispose when this widget is removed
    , disposeOnUpdated :: CD.Disposable -- things to dispose on updated
    , afterOnUpdated :: m () -- additional monadic action to take after a rerender
    , onUpdated :: Maybe (J.Callback (J.JSVal -> IO ()))
    , onRender :: Maybe (J.Callback (IO J.JSVal))
    } deriving (G.Generic)

mkPlan :: R.MonadReactor m => J.JSString -> m (Plan m)
mkPlan n = Plan
    <$> R.doGetComponent
    <*> R.doMkReactKey n
    <*> pure 0 -- ^ frameNum
    <*> pure mempty -- ^ disposeOnRemoved
    <*> pure mempty -- ^ disposeOnUpdated
    <*> pure (pure mempty) -- ^ afterOnUpdated
    <*> pure Nothing -- ^ callback
    <*> pure Nothing -- ^ render

-- Read-only
-- Using type synonym to a tuple for usages of 'alongside'.
type Frame m s = (Plan m, s)

plan :: Lens' (Frame m s) (Plan m)
plan = _1

model :: Lens' (Frame m s) s
model = _2

-- | Mutable
type Scene m v s = F.Obj v (Frame m s)

-- -- | If a new item was added, then we need to delay focusing until after the next render
-- focus :: (R.MonadReactor x m)
--     => IORef v
--     -> Lens' v (Frame x m s)
--     -> R.EventTarget
--     -> m ()
-- focus ref its j = do
--     R.doModifyIORef' ref (its.plan.field @"doOnUpdated" %~ (\g -> liftA2 const g (R.focus j)))
--     rerender ref its
