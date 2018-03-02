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
import Control.Monad.Trans
import Control.Monad.Trans.Cont
import Data.Diverse.Profunctor
import Data.Semigroup
import qualified GHC.Generics as G
import qualified Glazier.Core as Z
import qualified Glazier.React.Framework.Core.Display as Z
import qualified Glazier.React.Framework.Core.Model as Z

data Prototype m v s c = Prototype
    { display :: Z.FrameDisplay m s ()
    , finalizer :: Z.Finalizer m s
    , initializer :: Z.SceneInitializer m v s c
    } deriving (G.Generic, Functor)

instance Monad m => Applicative (Prototype m v s) where
    pure a = Prototype mempty Z.nulFinalizer (pure (pure a))
    (Prototype dis1 fin1 act1) <*> (Prototype dis2 fin2 act2) =
        Prototype
        (dis1 <> dis2)
        (fin1 `Z.alsoFinalizer` fin2)
        (liftA2 (<*>) act1 act2)

modifyDisplay :: (Z.FrameDisplay m s () -> Z.FrameDisplay m s ())
    -> Prototype m v s c -> Prototype m v s c
modifyDisplay f p = let disp = display p in p { display = f disp }
infixl 4 `modifyDisplay` -- like <$>

modifyFinalizer :: (Z.Finalizer m s -> Z.Finalizer m s)
    -> Prototype m v s c -> Prototype m v s c
modifyFinalizer f p = let fin = finalizer p in p { finalizer = f fin }
infixl 4 `modifyFinalizer` -- like <$>

modifyInitializer :: (Z.SceneInitializer m v s c1 -> Z.SceneInitializer m v s c2)
    -> Prototype m v s c1 -> Prototype m v s c2
modifyInitializer f p = let ini = initializer p in p { initializer = f ini }
infixl 4 `modifyInitializer` -- like <$>

------------------------------------------

-- | type restricted version of 'P.pmempty' for 'Prototype'
noopPrototype :: Monad m => Prototype m v s ()
noopPrototype = Prototype
        mempty
        Z.nulFinalizer
        Z.noopInitializer

-- | type restricted version of 'P.pmempty' for 'Prototype'
nulPrototype :: Monad m => Prototype m v s (Which '[])
nulPrototype = Prototype
        mempty
        Z.nulFinalizer
        Z.nulInitializer

alsoPrototype ::
    ( Monad m
    , ChooseBoth c1 c2 c3
    ) =>
    Prototype m v s (Which c1)
    -> Prototype m v s (Which c2)
    -> Prototype m v s (Which c3)
(Prototype dis1 fin1 act1) `alsoPrototype` (Prototype dis2 fin2 act2) =
        Prototype
        (dis1 <> dis2)
        (fin1 `Z.alsoFinalizer` fin2)
        (act1 `Z.alsoInitializer` act2)
infixr 6 `alsoPrototype` -- like mappend

memptyPrototype :: Monad m => Prototype m v s ()
memptyPrototype = Prototype mempty Z.nulFinalizer Z.memptyInitializer

mappendPrototype :: Monad m => Prototype m v s c -> Prototype m v s c -> Prototype m v s c
(Prototype dis1 fin1 act1) `mappendPrototype` (Prototype dis2 fin2 act2) =
        Prototype
        (dis1 <> dis2)
        (fin1 `Z.alsoFinalizer` fin2)
        (act1 `Z.mappendInitializer` act2)
infixr 6 `mappendPrototype` -- like mappend

thenPrototype :: Monad m => Prototype m v s () -> Prototype m v s c -> Prototype m v s c
(Prototype dis1 fin1 act1) `thenPrototype` (Prototype dis2 fin2 act2) =
        Prototype
        (dis1 <> dis2)
        (fin1 `Z.alsoFinalizer` fin2)
        (act1 `Z.thenInitializer` act2)
infixr 6 `thenPrototype` -- like mappend


-- | Modify prototype's reading environment @s1@ inside a larger @s2@
magnifyPrototype :: Lens' s2 s1  -> Prototype m v s1 c -> Prototype m v s2 c
magnifyPrototype sl (Prototype disp fin ini) = Prototype
    -- (Z.enlargeBuilder fi (view sl) bld)
    (magnify (alongside id sl) disp)
    (magnify sl fin)
    (Z.magnifyScene sl ini)

-- | Makes and initialzies a spec from a req.
-- Used by prototypes that contain other archetypes.
mkInitializedSpec :: Monad m
    => Z.MkSpec m r s
    -> Z.Initializer m s c
    -> r
    -> ContT () m (c, s)
mkInitializedSpec mkSpc ini r = do
    s <- lift $ Z.unMkSpec mkSpc r
    c <- ini s
    pure (c, s)

-- -- | Modify prototype's reading environment @s1@ inside a larger @s2@
-- magnifyPrototype :: Lens' s2 s1  -> Prototype m v s1 c -> Prototype m v s2 c
-- magnifyPrototype sl (Prototype disp fin ini) = Prototype
--     -- (Z.enlargeBuilder fi (view sl) bld)
--     (magnify (alongside id sl) disp)
--     (magnify sl fin)
--     (Z.magnifyScene sl ini)

-- -- | Convenience function to change a @Prototype m v s ()@ to @Prototype m v s (Which '[])@
-- terminatePrototype' :: Applicative m => Prototype m v s () -> Prototype m v s (Which '[])
-- terminatePrototype' = terminatePrototype @(Which '[])

-- terminatePrototype :: forall c m v s. Applicative m => Prototype m v s () -> Prototype m v s c
-- terminatePrototype = modifyInitializer (TE.terminate .)

-- -- | Apply isomorphisms of Info and Model to the prototype
-- enclose :: Functor m
--     => Iso' j i
--     -> (i' -> j')
--     -> Iso' t s
--     -> (s' -> t')
--     -> Prototype m v i s i' s' a x y
--     -> Prototype m v j t j' t' a x y
-- enclose ji ij ts st p =
--     let p'@(Prototype bld _ _ _ _) = Z.magnifyPrototype ts (Z.viaInfo ji p)
--     in p' { builder' = Z.mapBuilder ij st bld }

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
