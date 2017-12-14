{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Framework.Prototype where

import Control.Lens
import Data.Diverse
import Data.Semigroup (Semigroup(..))
import qualified Glazier.React.Framework.Activator as F
import qualified Glazier.React.Framework.Builder as F
import qualified Glazier.React.Framework.Core as F
import qualified Glazier.React.Framework.Display as F
import qualified Glazier.React.Framework.Handler as F
import qualified Parameterized.Data.Monoid as P
import qualified Parameterized.TypeLevel as P

newtype Prototype m v p s p' s' x a b y = Prototype {
    runPrototype ::
           ( F.Builder m p s p' s'
           , F.RefHandler m v (F.ComponentModel, s) x a b
           -- activator contains other prerequisites
           -- of executor, and actions that need to be handled
           , F.RefActivator m v (F.ComponentModel, s) y
           , F.Display m s ()
           )
    }

------------------------------------------

newtype PPrototype m v p s psxaby = PPrototype {
    runPPrototype :: Prototype m v p s (P.At0 psxaby) (P.At1 psxaby) (P.At2 psxaby) (P.At3 psxaby) (P.At4 psxaby) (P.At5 psxaby)
    }

type instance P.PNullary (PPrototype m v p s) (p', s', x, a, b, y) = Prototype m v p s p' s' x a b y

instance Monad m => P.PMEmpty (PPrototype m p s v) (Many '[], Many '[], Which '[], Which '[], Which '[], Which '[]) where
    pmempty = Prototype
        ( P.pmempty
        , P.pmempty
        , P.pmempty
        , mempty)

instance ( Monad m
         , p3 ~ Append p1 p2
         , s3 ~ Append s1 s2
         , x3 ~ AppendUnique x1 x2
         , Diversify x1 x3
         , Diversify x2 x3
         , a3 ~ Append a1 a2
         , b3 ~ AppendUnique b1 b2
         , Reinterpret a2 a3
         , a1 ~ Complement a3 a2
         , Diversify b1 b3
         , Diversify b2 b3
         , Reinterpret' y3 y1
         , Reinterpret' y3 y2
         , y3 ~ AppendUnique y1 y2
         ) => P.PSemigroup (PPrototype m v p s)
             (Many p1, Many s1, Which x1, Which a1, Which b1, Which y1)
             (Many p2, Many s2, Which x2, Which a2, Which b2, Which y2)
             (Many p3, Many s3, Which x3, Which a3, Which b3, Which y3) where
    (Prototype (bld1, hdl1, act1, disp1)) `pmappend` (Prototype (bld2, hdl2, act2, disp2)) =
        Prototype
        ( bld1 `P.pmappend` bld2
        , hdl1 `P.pmappend` hdl2
        , act1 `P.pmappend` act2
        , disp1 <> disp2
        )

------------------------------------------

building
    :: Monad m
    => F.Builder m p s p' s'
    -> Prototype m v p s p' s' (Which '[]) (Which '[]) (Which '[]) (Which '[])
building bld = Prototype
        ( bld
        , P.pmempty
        , P.pmempty
        , mempty)

refHandling
    :: Monad m
    => (F.RefHandler m v (F.ComponentModel, s) x a b)
    -> Prototype m v p s (Many '[]) (Many '[]) x a b (Which '[])
refHandling hdl = Prototype
        ( P.pmempty
        , hdl
        , P.pmempty
        , mempty)

refActivating
    :: Monad m
    => F.RefActivator m v (F.ComponentModel, s) y
    -> Prototype m v p s (Many '[]) (Many '[]) (Which '[]) (Which '[]) (Which '[]) y
refActivating act = Prototype
        ( P.pmempty
        , P.pmempty
        , act
        , mempty)

displaying
    :: Applicative m
    => F.Display m s ()
    -> Prototype m v p s (Many '[]) (Many '[]) (Which '[]) (Which '[]) (Which '[]) (Which '[])
displaying d = Prototype
        ( P.pmempty
        , P.pmempty
        , P.pmempty
        , d)

------------------------------------------

mapBuilder
    :: (F.Builder m p1 s p1' s1' -> F.Builder m p2 s p2' s2')
    -> Prototype m v p1 s p1' s1' x a b y
    -> Prototype m v p2 s p2' s2' x a b y
mapBuilder f (Prototype (bld, hdl, act, disp)) = Prototype
                   ( f bld
                   , hdl
                   , act
                   , disp)

mapRefHandler
    :: (   (F.RefHandler m v (F.ComponentModel, s) x1 a1 b1)
        -> (F.RefHandler m v (F.ComponentModel, s) x2 a2 b2))
    -> Prototype m v p s p' s' x1 a1 b1 y
    -> Prototype m v p s p' s' x2 a2 b2 y
mapRefHandler f (Prototype (bld, hdl, act, disp)) = Prototype
                   ( bld
                   , f hdl
                   , act
                   , disp)

mapRefActivator
    :: (F.RefActivator m v (F.ComponentModel, s) d1 -> F.RefActivator m v (F.ComponentModel, s) d2)
    -> Prototype m v p s p' s' a b c d1
    -> Prototype m v p s p' s' a b c d2
mapRefActivator f (Prototype (bld, hdl, act, disp)) = Prototype
                   ( bld
                   , hdl
                   , f act
                   , disp)

mapDisplay
    :: (F.Display m s () -> F.Display m s ())
    -> Prototype m v p s p' s' a b c d
    -> Prototype m v p s p' s' a b c d
mapDisplay f (Prototype (bld, hdl, act, disp)) = Prototype
                   ( bld
                   , hdl
                   , act
                   , f disp)

------------------------------------------

newtype PrototypeModeller m v p p' s' a b c d s = PrototypeModeller
    { runPrototypeModeller :: Prototype m v p s p' s' a b c d
    }

type instance F.Modeller (PrototypeModeller m v p p' s' a b c d) s = Prototype m v p s p' s' a b c d

instance F.ViaModel (PrototypeModeller m v p p' s' a b c d) where
    viaModel l (Prototype (bld, hdl, act, disp)) = Prototype
                   ( F.viaModel l bld
                   , F.viaModel (alongside id l) hdl
                   , F.viaModel (alongside id l) act
                   , F.viaModel l disp
                   )

------------------------------------------

newtype PrototypePlanner m v s p' s' a b c d p = PrototypePlanner
    { runPrototypePlanner :: Prototype m v p s p' s' a b c d
    }

type instance F.Planner (PrototypePlanner m v s p' s' a b c d) p = Prototype m v p s p' s' a b c d

instance F.ViaPlan (PrototypePlanner m v s p' s' a b c d) where
    viaPlan l (Prototype (bld, hdl, act, disp)) = Prototype
                   ( F.viaPlan l bld
                   , hdl
                   , act
                   , disp
                   )
