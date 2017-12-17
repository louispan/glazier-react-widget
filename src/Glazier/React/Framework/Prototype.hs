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

newtype Prototype m v p s p' s' x y a b = Prototype {
    runPrototype ::
           ( F.Display m s ()
           , F.Builder m p s p' s'
           -- activator contains other prerequisites
           -- of executor, and actions that need to be handled
           , F.RefActivator m v (F.ComponentModel, s) x
           , F.RefHandler m v (F.ComponentModel, s) y a b
           )
    }

------------------------------------------

newtype PPrototype m v p s psxyab = PPrototype {
    runPPrototype :: Prototype m v p s (P.At0 psxyab) (P.At1 psxyab) (P.At2 psxyab) (P.At3 psxyab) (P.At4 psxyab) (P.At5 psxyab)
    }

type instance P.PNullary (PPrototype m v p s) (p', s', x, y, a, b) = Prototype m v p s p' s' x y a b

instance Monad m => P.PMEmpty (PPrototype m p s v) (Many '[], Many '[], Which '[], Which '[], Which '[], Which '[]) where
    pmempty = Prototype
        ( mempty
        , P.pmempty
        , P.pmempty
        , P.pmempty
        )

instance ( Monad m
         , p3 ~ Append p1 p2
         , s3 ~ Append s1 s2
         , Reinterpret' x3 x1
         , Reinterpret' x3 x2
         , x3 ~ AppendUnique x1 x2
         , y3 ~ AppendUnique y1 y2
         , Diversify y1 y3
         , Diversify y2 y3
         , a3 ~ Append a1 a2
         , b3 ~ AppendUnique b1 b2
         , Reinterpret a2 a3
         , a1 ~ Complement a3 a2
         , Diversify b1 b3
         , Diversify b2 b3
         ) => P.PSemigroup (PPrototype m v p s)
             (Many p1, Many s1, Which x1, Which y1, Which a1, Which b1)
             (Many p2, Many s2, Which x2, Which y2, Which a2, Which b2)
             (Many p3, Many s3, Which x3, Which y3, Which a3, Which b3) where
    (Prototype (disp1, bld1, act1, hdl1)) `pmappend` (Prototype (disp2, bld2, act2, hdl2)) =
        Prototype
        ( disp1 <> disp2
        , bld1 `P.pmappend` bld2
        , act1 `P.pmappend` act2
        , hdl1 `P.pmappend` hdl2
        )

------------------------------------------

building
    :: Monad m
    => F.Builder m p s p' s'
    -> Prototype m v p s p' s' (Which '[]) (Which '[]) (Which '[]) (Which '[])
building bld = Prototype
        ( mempty
        , bld
        , P.pmempty
        , P.pmempty
        )

refHandling
    :: Monad m
    => (F.RefHandler m v (F.ComponentModel, s) y a b)
    -> Prototype m v p s (Many '[]) (Many '[]) (Which '[]) y a b
refHandling hdl = Prototype
        ( mempty
        , P.pmempty
        , P.pmempty
        , hdl
        )

refActivating
    :: Monad m
    => F.RefActivator m v (F.ComponentModel, s) x
    -> Prototype m v p s (Many '[]) (Many '[]) x (Which '[]) (Which '[]) (Which '[])
refActivating act = Prototype
        ( mempty
        , P.pmempty
        , act
        , P.pmempty
        )

displaying
    :: Applicative m
    => F.Display m s ()
    -> Prototype m v p s (Many '[]) (Many '[]) (Which '[]) (Which '[]) (Which '[]) (Which '[])
displaying d = Prototype
        ( d
        , P.pmempty
        , P.pmempty
        , P.pmempty
        )

------------------------------------------

mapBuilder
    :: (F.Builder m p1 s p1' s1' -> F.Builder m p2 s p2' s2')
    -> Prototype m v p1 s p1' s1' x y a b
    -> Prototype m v p2 s p2' s2' x y a b
mapBuilder f (Prototype (disp, bld, act, hdl)) = Prototype
                   ( disp
                   , f bld
                   , act
                   , hdl
                   )

mapRefHandler
    :: (   (F.RefHandler m v (F.ComponentModel, s) y1 a1 b1)
        -> (F.RefHandler m v (F.ComponentModel, s) y2 a2 b2))
    -> Prototype m v p s p' s' x y1 a1 b1
    -> Prototype m v p s p' s' x y2 a2 b2
mapRefHandler f (Prototype (disp, bld, act, hdl)) = Prototype
                   ( disp
                   , bld
                   , act
                   , f hdl
                   )

mapRefActivator
    :: (F.RefActivator m v (F.ComponentModel, s) x1 -> F.RefActivator m v (F.ComponentModel, s) x2)
    -> Prototype m v p s p' s' x1 y a b
    -> Prototype m v p s p' s' x2 y a b
mapRefActivator f (Prototype (disp, bld, act, hdl)) = Prototype
                   ( disp
                   , bld
                   , f act
                   , hdl
                   )

mapDisplay
    :: (F.Display m s () -> F.Display m s ())
    -> Prototype m v p s p' s' a b c d
    -> Prototype m v p s p' s' a b c d
mapDisplay f (Prototype (disp, bld, act, hdl)) = Prototype
                   ( f disp
                   , bld
                   , act
                   , hdl
                   )

------------------------------------------

newtype PrototypeModeller m v p p' s' x y a b s = PrototypeModeller
    { runPrototypeModeller :: Prototype m v p s p' s' x y a b
    }

type instance F.Modeller (PrototypeModeller m v p p' s' x y a b) s = Prototype m v p s p' s' x y a b

instance F.ViaModel (PrototypeModeller m v p p' s' x y a b) where
    viaModel l (Prototype (disp, bld, act, hdl)) = Prototype
                   ( F.viaModel l disp
                   , F.viaModel l bld
                   , F.viaModel (alongside id l) act
                   , F.viaModel (alongside id l) hdl
                   )

------------------------------------------

newtype PrototypePlanner m v s p' s' x y a b p = PrototypePlanner
    { runPrototypePlanner :: Prototype m v p s p' s' x y a b
    }

type instance F.Planner (PrototypePlanner m v s p' s' x y a b) p = Prototype m v p s p' s' x y a b

instance F.ViaPlan (PrototypePlanner m v s p' s' x y a b) where
    viaPlan l (Prototype (disp, bld, act, hdl)) = Prototype
                   ( disp
                   , F.viaPlan l bld
                   , act
                   , hdl
                   )
