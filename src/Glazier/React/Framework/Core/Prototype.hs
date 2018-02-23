{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
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

import Control.Lens
import Data.Diverse.Profunctor
import Data.Semigroup
import qualified GHC.Generics as G
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Core.Activator as R
import qualified Glazier.React.Framework.Core.Builder as R
import qualified Glazier.React.Framework.Core.Display as R
import qualified Glazier.React.Framework.Core.Finalizer as R
import qualified Glazier.React.Framework.Core.Handler as R
import qualified Glazier.React.Framework.Core.Model as R

data Prototype m v i s i' s' c a b = Prototype
    { builder :: R.Builder m i s i' s'
    , display :: R.FrameDisplay m s ()
    , finalizer :: R.Finalizer m s
    , activator :: R.SceneActivator m v s c
    , handler :: R.SceneHandler m v s a b
    } deriving (G.Generic)

-- | Use with 'R.constBuilder' to verify that the original builder is a
-- nulBuilder before replacing it.
mapBuilder :: (R.Builder m i s i1 s1 -> R.Builder m i' s i2 s2)
    -> Prototype m v i s i1 s1 c a b -> Prototype m v i' s i2 s2 c a b
mapBuilder f p = let bld = builder p in p { builder = f bld }
infixl 4 `mapBuilder` -- like <$>

mapDisplay :: (R.FrameDisplay m s () -> R.FrameDisplay m s ())
    -> Prototype m v i s i' s' c a b -> Prototype m v i s i' s' c a b
mapDisplay f p = let disp = display p in p { display = f disp }
infixl 4 `mapDisplay` -- like <$>

mapFinalizer :: (R.Finalizer m s -> R.Finalizer m s)
    -> Prototype m v i s i' s' c a b -> Prototype m v i s i' s' c a b
mapFinalizer f p = let fin = finalizer p in p { finalizer = f fin }
infixl 4 `mapFinalizer` -- like <$>

mapActivator :: (R.SceneActivator m v s c1 -> R.SceneActivator m v s c2)
    -> Prototype m v i s i' s' c1 a b -> Prototype m v i s i' s' c2 a b
mapActivator f p = let act = activator p in p { activator = f act }
infixl 4 `mapActivator` -- like <$>

mapHandler :: (R.SceneHandler m v s a1 b1 -> R.SceneHandler m v s a2 b2)
    -> Prototype m v i s i' s' c a1 b1 -> Prototype m v i s i' s' c a2 b2
mapHandler f p = let hdl = handler p in p { handler = f hdl }
infixl 4 `mapHandler` -- like <$>

------------------------------------------

-- | type restricted version of 'P.pmempty' for 'Prototype'
nulPrototype :: R.MonadReactor m => Prototype m v i s
    (Many '[])
    (Many '[])
    (Which '[])
    (Which '[])
    (Which '[])
nulPrototype = Prototype
        R.nulBuilder
        mempty
        R.nulFinalizer
        R.nulActivator
        R.nulHandler

--- | A friendlier constraint synonym for 'PBuilder' 'pmappend'.
type AndPrototype i1 i2 i3 s1 s2 s3 c1 c2 c3 a1 a2 a3 b1 b2 b3 =
    ( R.AndBuilder i1 i2 i3 s1 s2 s3
    , ChooseBoth c1 c2 c3
    , ChooseBetween a1 a2 a3 b1 b2 b3
    )
andPrototype ::
    ( Monad m
    , AndPrototype i1 i2 i3 s1 s2 s3 c1 c2 c3 a1 a2 a3 b1 b2 b3
    ) =>
    Prototype m v i s (Many i1) (Many s1) (Which c1) (Which a1) (Which b1)
    -> Prototype m v i s (Many i2) (Many s2) (Which c2) (Which a2) (Which b2)
    -> Prototype m v i s (Many i3) (Many s3) (Which c3) (Which a3) (Which b3)
(Prototype bld1 dis1 fin1 act1 hdl1) `andPrototype` (Prototype bld2 dis2 fin2 act2 hdl2) =
        Prototype
        (bld1 `R.andBuilder` bld2)
        (dis1 <> dis2)
        (fin1 `R.andFinalizer` fin2)
        (act1 `R.andActivator` act2)
        (hdl1 `R.orHandler` hdl2)
infixr 6 `andPrototype` -- like mappend

-- -- | A friendlier constraint synonym for 'PBuilder' 'pmappend'.
-- type AndPrototype m i1 i2 i3 s1 s2 s3 c1 c2 c3 a1 a2 a3 b1 b2 b3 =
--     ( R.AndBuilder m i1 i2 i3 s1 s2 s3
--     , ChooseBoth c1 c2 c3
--     , ChooseBetween a1 a2 a3 b1 b2 b3
--     )

-- instance ( R.MonadReactor m
--          , AndPrototype m i1 i2 i3 s1 s2 s3 c1 c2 c3 a1 a2 a3 b1 b2 b3
--          ) => P.PSemigroup (PPrototype m v i s)
--              (i1, s1, Which c1, Which a1, Which b1)
--              (i2, s2, Which c2, Which a2, Which b2)
--              (i3, s3, Which c3, Which a3, Which b3) where
--     (Prototype bld1 dis1 fin1 act1 hdl1) `pmappend` (Prototype bld2 dis2 fin2 act2 hdl2) =
--         Prototype
--         (bld1 `R.andBuilder` bld2)
--         (dis1 <> dis2)
--         (fin1 `R.andFinalizer` fin2)
--         (act1 `R.andActivator` act2)
--         (hdl1 `R.orHandler` hdl2)

-- -- | type restricted version of 'P.pmappend' for 'Prototype'
-- andPrototype ::
--     ( R.MonadReactor m
--     , AndPrototype m i1 i2 i3 s1 s2 s3 c1 c2 c3 a1 a2 a3 b1 b2 b3
--     )
--     => Prototype m v i s i1 s1 (Which c1) (Which a1) (Which b1)
--     -> Prototype m v i s i2 s2 (Which c2) (Which a2) (Which b2)
--     -> Prototype m v i s i3 s3 (Which c3) (Which a3) (Which b3)
-- andPrototype = P.pmappend
-- infixr 6 `andPrototype` -- like mappend

------------------------------------------

newtype PrototypeOnSpec m v i i' s' c a b s = PrototypeOnSpec
    { unPrototypeOnSpec :: Prototype m v i s i' s' c a b
    }

instance R.ViaSpec (PrototypeOnSpec m v i i' s' c a b) where
    type OnSpec (PrototypeOnSpec m v i i' s' c a b) s = Prototype m v i s i' s' c a b
    viaSpec l (Prototype bld dis fin act hdl) = Prototype
        (R.viaSpec l bld)
        (R.viaSpec (alongside id l) dis)
        (R.viaSpec l fin)
        (R.viaObj (alongside id l) act)
        (R.viaObj (alongside id l) hdl)

------------------------------------------

newtype PrototypeOnInfo m v s i' s' c a b i = PrototypeOnInfo
    { unPrototypeOnInfo :: Prototype m v i s i' s' c a b
    }

instance R.ViaInfo (PrototypeOnInfo m v s i' s' c a b) where
    type OnInfo (PrototypeOnInfo m v s i' s' c a b) i = Prototype m v i s i' s' c a b
    viaInfo l (Prototype bld dis fin act hdl) = Prototype
        (R.viaInfo l bld)
        dis
        fin
        act
        hdl

-- -- | Apply isomorphisms of Info and Model to the prototype
-- enclose :: Functor m
--     => Iso' j i
--     -> (i' -> j')
--     -> Iso' t s
--     -> (s' -> t')
--     -> Prototype m v i s i' s' a x y
--     -> Prototype m v j t j' t' a x y
-- enclose ji ij ts st p =
--     let p'@(Prototype bld _ _ _ _) = R.viaSpec ts (R.viaInfo ji p)
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

-- | Wrap a prototype's info and model as an item inside a Many.
toItemPrototype
    :: ( Functor m
       , HasItem' s1 s2
       , HasItem' i1 i2
       )
    => Prototype m v i1 s1 i' s' a x y
    -> Prototype m v i2 s2 (Many '[i']) (Many '[s']) a x y
toItemPrototype p =
    let p'@(Prototype bld _ _ _ _) = R.viaSpec item' (R.viaInfo item' p)
    in p' { builder = bimap single single bld }
