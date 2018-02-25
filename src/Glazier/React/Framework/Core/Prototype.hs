{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Framework.Core.Prototype where

import Control.Applicative
import Control.Lens
import Data.Diverse.Profunctor
import Data.Semigroup
import qualified GHC.Generics as G
import qualified Glazier.React.Framework.Core.Activator as R
import qualified Glazier.React.Framework.Core.Display as R
import qualified Glazier.React.Framework.Core.Finalizer as R
import qualified Glazier.React.Framework.Core.Model as R
import qualified Glazier.React.Framework.Core.Trigger as R

data Prototype m v s c = Prototype
    { display :: R.FrameDisplay m s ()
    , finalizer :: R.Finalizer m s
    , activator :: R.SceneActivator m v s c
    } deriving (G.Generic, Functor)

instance Monad m => Applicative (Prototype m v s) where
    pure a = Prototype mempty R.nulFinalizer (pure (pure a))
    (Prototype dis1 fin1 act1) <*> (Prototype dis2 fin2 act2) =
        Prototype
        (dis1 <> dis2)
        (fin1 `R.andFinalizer` fin2)
        (liftA2 (<*>) act1 act2)

mapDisplay :: (R.FrameDisplay m s () -> R.FrameDisplay m s ())
    -> Prototype m v s c -> Prototype m v s c
mapDisplay f p = let disp = display p in p { display = f disp }
infixl 4 `mapDisplay` -- like <$>

mapFinalizer :: (R.Finalizer m s -> R.Finalizer m s)
    -> Prototype m v s c -> Prototype m v s c
mapFinalizer f p = let fin = finalizer p in p { finalizer = f fin }
infixl 4 `mapFinalizer` -- like <$>

mapActivator :: (R.SceneActivator m v s c1 -> R.SceneActivator m v s c2)
    -> Prototype m v s c1 -> Prototype m v s c2
mapActivator f p = let act = activator p in p { activator = f act }
infixl 4 `mapActivator` -- like <$>

------------------------------------------

-- | type restricted version of 'P.pmempty' for 'Prototype'
nulPrototype :: Monad m => Prototype m v s (Which '[])
nulPrototype = Prototype
        mempty
        R.nulFinalizer
        R.nulActivator

andPrototype ::
    ( Monad m
    , ChooseBoth c1 c2 c3
    ) =>
    Prototype m v s (Which c1)
    -> Prototype m v s (Which c2)
    -> Prototype m v s (Which c3)
(Prototype dis1 fin1 act1) `andPrototype` (Prototype dis2 fin2 act2) =
        Prototype
        (dis1 <> dis2)
        (fin1 `R.andFinalizer` fin2)
        (act1 `R.andActivator` act2)
infixr 6 `andPrototype` -- like mappend

memptyPrototype :: Monad m => Prototype m v s ()
memptyPrototype = Prototype mempty R.nulFinalizer R.memptyActivator

mappendPrototype :: Monad m => Prototype m v s c -> Prototype m v s c -> Prototype m v s c
(Prototype dis1 fin1 act1) `mappendPrototype` (Prototype dis2 fin2 act2) =
        Prototype
        (dis1 <> dis2)
        (fin1 `R.andFinalizer` fin2)
        (act1 `R.mappendActivator` act2)
infixr 6 `mappendPrototype` -- like mappend


-- | Modify prototype's reading environment @s1@ inside a larger @s2@
magnifyPrototype :: Lens' s2 s1  -> Prototype m v s1 c -> Prototype m v s2 c
magnifyPrototype sl (Prototype disp fin act) = Prototype
    -- (R.enlargeBuilder fi (view sl) bld)
    (magnify (alongside id sl) disp)
    (magnify sl fin)
    (R.magnifyScene sl act)

-- | Convenience function to change a @Prototype m v s ()@ to @Prototype m v s (Which '[])@
terminal :: Applicative m => Prototype m v s () -> Prototype m v s (Which '[])
terminal = mapActivator (R.terminate' .)

-- -- | Apply isomorphisms of Info and Model to the prototype
-- enclose :: Functor m
--     => Iso' j i
--     -> (i' -> j')
--     -> Iso' t s
--     -> (s' -> t')
--     -> Prototype m v i s i' s' a x y
--     -> Prototype m v j t j' t' a x y
-- enclose ji ij ts st p =
--     let p'@(Prototype bld _ _ _ _) = R.magnifyPrototype ts (R.viaInfo ji p)
--     in p' { builder' = R.mapBuilder ij st bld }

-- toTaggedPrototype :: forall t m v i s i' s' a x y.
--     Functor m
--     => Prototype m v i s i' s' a x y
--     -> Prototype m v (Tagged t i) (Tagged t s) (Tagged t i') (Tagged t s') a x y
-- toTaggedPrototype p =
--     let ts :: Iso' (Tagged t s) s
--         ts = iso unTagged Tagged
--         ji :: Iso' (Tagged t i) i
--         ji = iso unTagged Tagged
--     in enclose ji (Tagged @t) ts (Tagged @t) p

-- -- | Wrap a prototype's info and model as an item inside a Many.
-- toItemPrototype :: ( Functor m)
--     => (i2 -> i1)
--     -> Lens' s2 s1
--     -> Prototype m v i1 s1 i' s' c
--     -> Prototype m v i2 s2 (Many '[i']) (Many '[s']) c
-- toItemPrototype fi sl = mapBuilder (bimap single single)
--     . enlargePrototype fi sl
