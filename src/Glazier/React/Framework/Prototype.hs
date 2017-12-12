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

newtype Prototype m v p s p' s' a b c = Prototype {
    runPrototype ::
           ( F.Builder m p s p' s'
           , F.RefHandler m v (F.ComponentModel, s) a b
           -- activator contains other prerequisites
           -- of executor, and actions that need to be handled
           , F.RefActivator m v (F.ComponentModel, s) c
           , F.Display m s ()
           )
    }

------------------------------------------

newtype PPrototype m v p s psabc = PPrototype {
    runPPrototype :: Prototype m v p s (P.At0 psabc) (P.At1 psabc) (P.At2 psabc) (P.At3 psabc) (P.At4 psabc)
    }

type instance P.PNullary (PPrototype m v p s) (p', s', a, b, c) = Prototype m v p s p' s' a b c

instance Monad m => P.PMEmpty (PPrototype m p s v) (Many '[], Many '[], Which '[], Which '[], Which '[]) where
    pmempty = Prototype
        ( P.pmempty
        , P.pmempty
        , P.pmempty
        , mempty)

instance ( Monad m
         , p3 ~ Append p1 p2
         , s3 ~ Append s1 s2
         , a3 ~ Append a1 a2
         , b3 ~ AppendUnique b1 b2
         , Reinterpret a2 a3
         , a1 ~ Complement a3 a2
         , Diversify b1 b3
         , Diversify b2 b3
         , Reinterpret' c3 c1
         , Reinterpret' c3 c2
         , c3 ~ AppendUnique c1 c2
         ) => P.PSemigroup (PPrototype m v p s)
             (Many p1, Many s1, Which a1, Which b1, Which c1)
             (Many p2, Many s2, Which a2, Which b2, Which c2)
             (Many p3, Many s3, Which a3, Which b3, Which c3) where
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
    -> Prototype m v p s p' s' (Which '[]) (Which '[]) (Which '[])
building bld = Prototype
        ( bld
        , P.pmempty
        , P.pmempty
        , mempty)

refHandling
    :: Monad m
    => F.RefHandler m v (F.ComponentModel, s) a b
    -> Prototype m v p s (Many '[]) (Many '[]) a b (Which '[])
refHandling hdl = Prototype
        ( P.pmempty
        , hdl
        , P.pmempty
        , mempty)

refActivating
    :: Monad m
    => F.RefActivator m v (F.ComponentModel, s) c
    -> Prototype m v p s (Many '[]) (Many '[]) (Which '[]) (Which '[]) c
refActivating act = Prototype
        ( P.pmempty
        , P.pmempty
        , act
        , mempty)

displaying
    :: Applicative m
    => F.Display m s ()
    -> Prototype m v p s (Many '[]) (Many '[]) (Which '[]) (Which '[]) (Which '[])
displaying d = Prototype
        ( P.pmempty
        , P.pmempty
        , P.pmempty
        , d)

------------------------------------------

mapBuilder
    :: (F.Builder m p1 s p1' s1' -> F.Builder m p2 s p2' s2')
    -> Prototype m v p1 s p1' s1' a b c
    -> Prototype m v p2 s p2' s2' a b c
mapBuilder f (Prototype (bld, hdl, act, disp)) = Prototype
                   ( f bld
                   , hdl
                   , act
                   , disp)

mapRefHandler
    :: (F.RefHandler m v (F.ComponentModel, s) a1 b1 -> F.RefHandler m v (F.ComponentModel, s) a2 b2)
    -> Prototype m v p s p' s' a1 b1 c
    -> Prototype m v p s p' s' a2 b2 c
mapRefHandler f (Prototype (bld, hdl, act, disp)) = Prototype
                   ( bld
                   , f hdl
                   , act
                   , disp)

mapRefActivator
    :: (F.RefActivator m v (F.ComponentModel, s) c1 -> F.RefActivator m v (F.ComponentModel, s) c2)
    -> Prototype m v p s p' s' a b c1
    -> Prototype m v p s p' s' a b c2
mapRefActivator f (Prototype (bld, hdl, act, disp)) = Prototype
                   ( bld
                   , hdl
                   , f act
                   , disp)

mapDisplay
    :: (F.Display m s () -> F.Display m s ())
    -> Prototype m v p s p' s' a b c
    -> Prototype m v p s p' s' a b c
mapDisplay f (Prototype (bld, hdl, act, disp)) = Prototype
                   ( bld
                   , hdl
                   , act
                   , f disp)

------------------------------------------

newtype PrototypeModeller m v p p' s' a b c s = PrototypeModeller
    { runPrototypeModeller :: Prototype m v p s p' s' a b c
    }

type instance F.Modeller (PrototypeModeller m v p p' s' a b c) s = Prototype m v p s p' s' a b c

instance F.ViaModel (PrototypeModeller m v p p' s' a b c) where
    viaModel l (Prototype (bld, hdl, act, disp)) = Prototype
                   ( F.viaModel l bld
                   , F.viaModel (alongside id l) hdl
                   , F.viaModel (alongside id l) act
                   , F.viaModel l disp
                   )

------------------------------------------

newtype PrototypePlanner m v s p' s' a b c p = PrototypePlanner
    { runPrototypePlanner :: Prototype m v p s p' s' a b c
    }

type instance F.Planner (PrototypePlanner m v s p' s' a b c) p = Prototype m v p s p' s' a b c

instance F.ViaPlan (PrototypePlanner m v s p' s' a b c) where
    viaPlan l (Prototype (bld, hdl, act, disp)) = Prototype
                   ( F.viaPlan l bld
                   , hdl
                   , act
                   , disp
                   )
