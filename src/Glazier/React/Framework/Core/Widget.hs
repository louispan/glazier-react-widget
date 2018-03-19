{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Framework.Core.Widget where

import Control.Lens
import Data.Semigroup
import qualified GHC.Generics as G
import Glazier.React.Framework.Core.Gadget
import Glazier.React.Framework.Core.MkId
import Glazier.React.Framework.Core.Model
import Glazier.React.Framework.Core.Window

-- | Unforunately, because each widget contains callbacks
-- that has to be cleaned manually, we can't just rely on the garbage collector.
-- This contains 'Disposable' to be called on the *next* render.
-- This must be called before removing widgets from a containing model.
-- type Finalizer s m = s -> Ap m CD.Disposable

-- type Initializer x s m = (Widget x s m, MonadCont m)
-- type Handler x s m = (Widget x s m, MonadCont m)

data Widget w x s m c = Widget
    { window :: WindowT x s m ()
    , gadget :: GadgetT w x s m c
    } deriving (G.Generic, Functor)

makeLenses ''Widget

mapWidget2 :: Monad m
    => (GadgetT w x s m c1 -> GadgetT w x s m c2 -> GadgetT w x s m c3)
    -> Widget w x s m c1 -> Widget w x s m c2 -> Widget w x s m c3
mapWidget2 f (Widget dis1 ini1) (Widget dis2 ini2) =
    Widget
    (dis1 <> dis2)
    (f ini1 ini2)

------------------------------------------

instance Monad m => Applicative (Widget w x s m) where
    pure a = Widget mempty (pure a)
    (<*>) = mapWidget2 (<*>)

-- merge ContT together by pre-firing the left ContT's output.
-- That is, the resultant ContT will fire the output twice.
instance (Semigroup c, Monad m) => Semigroup (Widget w x s m c) where
    (<>) = mapWidget2 (<>)

instance (Monoid c, Monad m) => Monoid (Widget w x s m c) where
    mempty = Widget mempty mempty
    mappend = mapWidget2 mappend

instance Monad m => EnlargeModel (Widget w x a m r) where
    type WithEnlargedModel (Widget w x a m r) s = Widget w x s m r
    type EnlargingModel (Widget w x a m r) = a
    enlargeModel l (Widget disp ini) = Widget (enlargeModel l disp) (enlargeModel l ini)

instance Monad m => EnlargePlan (Widget w x s m r) where
    enlargePlan l (Widget disp ini) = Widget (enlargePlan l disp) (enlargePlan l ini)

-- | Wrap this widget inside another 'ShimComponent' with its own 'Plan'
-- This means that its 'dirty' state and 'Rerender' is isolated from other 'Widget's
archetype :: Monad m => PlanId -> Widget w x s m r -> Widget w x s m r
archetype pid = enlargePlan (_plans.ix pid)

data MkArchetype w x s = MkArchetype
    (ReifiedTraversal' w (Scene x s))
    (Window x s ())

-- magnifyMethod :: Monad m
--     => LensLike' f s a -> MethodT w x s m c1 -> MethodT w x s m c1
-- magnifyDisplay l disp = magnify (editScene l) disp

--    (magnify (to $ \(Traversal t) -> Traversal (t . l)) ini)

-- -- | Makes and initialzies a spec from a req.
-- -- Used by prototypes that contain other archetypes.
-- mkInitializedSpec :: Monad m
--     => (r -> m s)
--     -> MethodT s m c
--     -> MethodT r m (c, s)
-- mkInitializedSpec mkSpc ini = do
--     r <- ask
--     s <- lift $ lift $ mkSpc r
--     c <- magnify (to (const s)) ini
--     pure (c, s)


-- -- obviousHandler :: Handler s m a b -> Handler s m (Which '[a]) b
-- -- obviousHandler hdl = hdl . obvious

-- -- contramapHandler :: (a1 -> a2) -> Handler s m a2 b -> Handler s m a1 b
-- -- contramapHandler f hdl = hdl . f

-- -- mapHandler :: (b1 -> b2) -> Handler s m a b1 -> Handler s m a b2
-- -- mapHandler = fmap . fmap

-- -- memptyHandler :: Applicative m => Handler s m a b
-- -- memptyHandler = mempty

-- -- mappendHandler :: Applicative m => Handler s m a b -> Handler s m a b -> Handler s m a b
-- -- mappendHandler = mappend
-- -- infixr 6 `mappendHandler` -- like mappend

-- -- ignoreHandler :: forall a m s. Applicative m => Handler s m a ()
-- -- ignoreHandler = (const @_ @a) mempty

-- -- arrowHandler :: (a -> b) -> Handler s m a b
-- -- arrowHandler f = rk $ arr f

-- -- -- Chain the output from one handler into the input of the other.
-- --     Handler s m a b
-- --     -> Handler s m b c
-- --     -> Handler s m a c
-- -- -- intoH f g = f & E.rk2 (>>>) $ g
-- -- intoH f g = f >=> g

-- -- -- Chain the output from one handler into the input of the other
-- -- -- as much as possible. Any unhandled output is forwarded.
-- -- intoH' :: (Injected a2 b1 b2 b3)
-- --     => Handler s m a (Which b1)
-- --     -> Handler s m (Which a2) (Which b2)
-- --     -> Handler s m a (Which b3)
-- -- intoH' f g = f >=> E.underK1 injected g

-- -- -- Run th left handler and then the right handler with the same input,
-- -- -- and only fire events from the second input.
-- -- thenH :: Handler s m a () -> Handler s m a b -> Handler s m a b
-- -- thenH = ($!*>)

-- -- -- Run left and also the right handler with the same input, and combine the output type
-- -- -- A binary associative function for 'nulHandler'.
-- -- alsoH :: (Applicative m, ChooseBoth b1 b2 b3)
-- --     => Handler s m a (Which b1)
-- --     -> Handler s m a (Which b2)
-- --     -> Handler s m a (Which b3)
-- -- alsoH = liftA2 also
-- -- infixr 6 `alsoH` -- like mappend

-- -- maybeH :: Applicative m
-- --     => Handler s m a b
-- --     -> Handler s m (Maybe a) b
-- -- maybeH hdl = maybe mempty hdl
