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
import qualified Glazier.React.Framework.Core.Obj as R

data Prototype m v i s i' s' c = Prototype
    { builder :: R.Builder m i s i' s'
    , display :: R.FrameDisplay m s ()
    , finalizer :: R.Finalizer m s
    , activator :: R.SceneActivator m v s c
    } deriving (G.Generic)

-- | Use with 'R.constBuilder' to verify that the original builder is a
-- nulBuilder before replacing it.
mapBuilder :: (R.Builder m i s i1 s1 -> R.Builder m i' s i2 s2)
    -> Prototype m v i s i1 s1 c -> Prototype m v i' s i2 s2 c
mapBuilder f p = let bld = builder p in p { builder = f bld }
infixl 4 `mapBuilder` -- like <$>

mapDisplay :: (R.FrameDisplay m s () -> R.FrameDisplay m s ())
    -> Prototype m v i s i' s' c -> Prototype m v i s i' s' c
mapDisplay f p = let disp = display p in p { display = f disp }
infixl 4 `mapDisplay` -- like <$>

mapFinalizer :: (R.Finalizer m s -> R.Finalizer m s)
    -> Prototype m v i s i' s' c -> Prototype m v i s i' s' c
mapFinalizer f p = let fin = finalizer p in p { finalizer = f fin }
infixl 4 `mapFinalizer` -- like <$>

mapActivator :: (R.SceneActivator m v s c1 -> R.SceneActivator m v s c2)
    -> Prototype m v i s i' s' c1 -> Prototype m v i s i' s' c2
mapActivator f p = let act = activator p in p { activator = f act }
infixl 4 `mapActivator` -- like <$>

-- mapHandler :: (R.SceneHandler m v s a1 b1 -> R.SceneHandler m v s a2 b2)
--     -> Prototype m v i s i' s' c a1 b1 -> Prototype m v i s i' s' c a2 b2
-- mapHandler f p = let hdl = handler p in p { handler = f hdl }
-- infixl 4 `mapHandler` -- like <$>

------------------------------------------

-- | type restricted version of 'P.pmempty' for 'Prototype'
nulPrototype :: R.MonadReactor m => Prototype m v i s
    (Many '[])
    (Many '[])
    (Which '[])
nulPrototype = Prototype
        R.nulBuilder
        mempty
        R.nulFinalizer
        R.nulActivator

--- | A friendlier constraint synonym for 'PBuilder' 'pmappend'.
type AndPrototype i1 i2 i3 s1 s2 s3 c1 c2 c3 =
    ( R.AndBuilder i1 i2 i3 s1 s2 s3
    , ChooseBoth c1 c2 c3
    )
andPrototype ::
    ( Monad m
    , AndPrototype i1 i2 i3 s1 s2 s3 c1 c2 c3
    ) =>
    Prototype m v i s (Many i1) (Many s1) (Which c1)
    -> Prototype m v i s (Many i2) (Many s2) (Which c2)
    -> Prototype m v i s (Many i3) (Many s3) (Which c3)
(Prototype bld1 dis1 fin1 act1) `andPrototype` (Prototype bld2 dis2 fin2 act2) =
        Prototype
        (bld1 `R.andBuilder` bld2)
        (dis1 <> dis2)
        (fin1 `R.andFinalizer` fin2)
        (act1 `R.andActivator` act2)
infixr 6 `andPrototype` -- like mappend

-- | Modify prototype's reading environment @i1@ and @s1@ inside a larger @i2@ @s2@
byPrototype
    :: (i2 -> i1)
    -> Lens' s2 s1
    -> Prototype m v i1 s1 i' s' c
    -> Prototype m v i2 s2 i' s' c
byPrototype fi l = viaSpec l . viaInfo fi

-- | Modify prototype's reading environment @s1@ inside a larger @s2@
viaSpec :: Lens' s2 s1  -> Prototype m v i s1 i' s' c -> Prototype m v i s2 i' s' c
viaSpec l (Prototype bld dis fin act) = Prototype
    (R.byBuilder id (view l) bld)
    (dis . view (alongside id l))
    (fin . view l)
    (act . R.edit (alongside id l))

-- | Modify prototype's reading environment @i1@ inside a larger @i2@
viaInfo :: (j -> i) -> Prototype m v i s i' s' c -> Prototype m v j s i' s' c
viaInfo fi = mapBuilder (R.byBuilder fi id)

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
       )
    => (i2 -> i1)
    -> Lens' s2 s1
    -> Prototype m v i1 s1 i' s' c
    -> Prototype m v i2 s2 (Many '[i']) (Many '[s']) c
toItemPrototype fi l = mapBuilder (bimap single single)
    . viaSpec l . viaInfo fi
