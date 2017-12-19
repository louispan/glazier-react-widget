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
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Activator as F
import qualified Glazier.React.Framework.Builder as F
import qualified Glazier.React.Framework.Core as F
import qualified Glazier.React.Framework.Display as F
import qualified Glazier.React.Framework.Handler as F
import qualified Parameterized.Data.Monoid as P
import qualified Parameterized.TypeLevel as P

newtype Prototype m v p s p' s' a b x c = Prototype {
    runPrototype ::
           ( F.Display m s ()
           , F.Builder m p s p' s'
           , F.RefHandler m v (F.ComponentModel, s) a b
           -- activator contains other prerequisites
           -- of executor, and actions that need to be handled
           , F.RefActivator m v (F.ComponentModel, s) x c
           )
    }

------------------------------------------

newtype PPrototype m v p s psabxc = PPrototype {
    runPPrototype :: Prototype m v p s (P.At0 psabxc) (P.At1 psabxc) (P.At2 psabxc) (P.At3 psabxc) (P.At4 psabxc) (P.At5 psabxc)
    }

type instance P.PNullary (PPrototype m v p s) (p', s', a, b, x, c) = Prototype m v p s p' s' a b x c

instance R.MonadReactor x m => P.PMEmpty (PPrototype m p s v) (Many '[], Many '[], Which '[], Which '[], x, Which '[]) where
    pmempty = Prototype
        ( mempty
        , P.pmempty
        , P.pmempty
        , P.pmempty
        )

instance ( R.MonadReactor x m
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
             (Many p1, Many s1, Which a1, Which b1, x, Which c1)
             (Many p2, Many s2, Which a2, Which b2, x, Which c2)
             (Many p3, Many s3, Which a3, Which b3, x, Which c3) where
    (Prototype (disp1, bld1, hdl1, act1)) `pmappend` (Prototype (disp2, bld2, hdl2, act2)) =
        Prototype
        ( disp1 <> disp2
        , bld1 `P.pmappend` bld2
        , hdl1 `P.pmappend` hdl2
        , act1 `P.pmappend` act2
        )

------------------------------------------

building
    :: Monad m
    => F.Builder m p s p' s'
    -> Prototype m v p s p' s' (Which '[]) (Which '[]) x (Which '[])
building bld = Prototype
        ( mempty
        , bld
        , P.pmempty
        , P.pmempty
        )

refHandling
    :: Monad m
    => (F.RefHandler m v (F.ComponentModel, s) a b)
    -> Prototype m v p s (Many '[]) (Many '[]) a b x (Which '[])
refHandling hdl = Prototype
        ( mempty
        , P.pmempty
        , hdl
        , P.pmempty
        )

refActivating
    :: Monad m
    => F.RefActivator m v (F.ComponentModel, s) x c
    -> Prototype m v p s (Many '[]) (Many '[]) (Which '[]) (Which '[]) x c
refActivating act = Prototype
        ( mempty
        , P.pmempty
        , P.pmempty
        , act
        )

displaying
    :: Applicative m
    => F.Display m s ()
    -> Prototype m v p s (Many '[]) (Many '[]) (Which '[]) (Which '[]) x (Which '[])
displaying d = Prototype
        ( d
        , P.pmempty
        , P.pmempty
        , P.pmempty
        )

------------------------------------------

mapBuilder
    :: (F.Builder m p1 s p1' s1' -> F.Builder m p2 s p2' s2')
    -> Prototype m v p1 s p1' s1' a b x c
    -> Prototype m v p2 s p2' s2' a b x c
mapBuilder f (Prototype (disp, bld, hdl, act)) = Prototype
                   ( disp
                   , f bld
                   , hdl
                   , act
                   )

mapRefHandler
    :: (   (F.RefHandler m v (F.ComponentModel, s) a1 b1)
        -> (F.RefHandler m v (F.ComponentModel, s) a2 b2))
    -> Prototype m v p s p' s' a1 b1 x c
    -> Prototype m v p s p' s' a2 b2 x c
mapRefHandler f (Prototype (disp, bld, hdl, act)) = Prototype
                   ( disp
                   , bld
                   , f hdl
                   , act
                   )

mapRefActivator
    :: (F.RefActivator m v (F.ComponentModel, s) x c1 -> F.RefActivator m v (F.ComponentModel, s) x c2)
    -> Prototype m v p s p' s' a b x c1
    -> Prototype m v p s p' s' a b x c2
mapRefActivator f (Prototype (disp, bld, hdl, act)) = Prototype
                   ( disp
                   , bld
                   , hdl
                   , f act
                   )

mapDisplay
    :: (F.Display m s () -> F.Display m s ())
    -> Prototype m v p s p' s' a b x c
    -> Prototype m v p s p' s' a b x c
mapDisplay f (Prototype (disp, bld, hdl, act)) = Prototype
                   ( f disp
                   , bld
                   , hdl
                   , act
                   )

------------------------------------------

newtype PrototypeModeller m v p p' s' a b x c s = PrototypeModeller
    { runPrototypeModeller :: Prototype m v p s p' s' a b x c
    }

type instance F.Modeller (PrototypeModeller m v p p' s' a b x c) s = Prototype m v p s p' s' a b x c

instance F.ViaModel (PrototypeModeller m v p p' s' a b x c) where
    viaModel l (Prototype (disp, bld, hdl, act)) = Prototype
                   ( F.viaModel l disp
                   , F.viaModel l bld
                   , F.viaModel (alongside id l) hdl
                   , F.viaModel (alongside id l) act
                   )

------------------------------------------

newtype PrototypePlanner m v s p' s' a b x c p = PrototypePlanner
    { runPrototypePlanner :: Prototype m v p s p' s' a b x c
    }

type instance F.Planner (PrototypePlanner m v s p' s' a b x c) p = Prototype m v p s p' s' a b x c

instance F.ViaPlan (PrototypePlanner m v s p' s' a b x c) where
    viaPlan l (Prototype (disp, bld, hdl, act)) = Prototype
                   ( disp
                   , F.viaPlan l bld
                   , hdl
                   , act
                   )
