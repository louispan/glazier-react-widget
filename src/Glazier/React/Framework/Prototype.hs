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
import qualified Glazier.React.Framework.Builder as F
import qualified Glazier.React.Framework.Core as F
import qualified Glazier.React.Framework.Disposer as F
import qualified Glazier.React.Framework.Window as F
import qualified Glazier.React.Framework.Executor as F
import qualified Parameterized.Data.Monoid as P
import qualified Parameterized.TypeLevel as P

newtype Prototype m v i s i' s' x c a b = Prototype {
    runPrototype ::
           ( F.Disposer m s
           , F.Window m (F.ComponentPlan x m, s) ()
           , F.Builder m i s i' s'
           , F.RefExecutor m v (F.ComponentPlan x m, s) x c a b
           )
    }

------------------------------------------

newtype PPrototype m v i s x iscab = PPrototype {
    runPPrototype :: Prototype m v i s (P.At0 iscab) (P.At1 iscab) x (P.At2 iscab) (P.At3 iscab) (P.At4 iscab)
    }

type instance P.PNullary (PPrototype m v i s x) (i', s', c, a, b) = Prototype m v i s i' s' x c a b

instance R.MonadReactor x m => P.PMEmpty (PPrototype m i s v x) (Many '[], Many '[], Which '[], Which '[], Which '[]) where
    pmempty = Prototype
        ( mempty
        , mempty
        , P.pmempty
        , P.pmempty
        )

instance ( R.MonadReactor x m
         , i3 ~ Append i1 i2
         , s3 ~ Append s1 s2
         , a3 ~ Append a1 a2
         , b3 ~ AppendUnique b1 b2
         , Reinterpret a2 a3
         , a1 ~ Complement a3 a2
         , Diversify b1 b3
         , Diversify b2 b3
         , Diversify c1 c3
         , Diversify c2 c3
         , c3 ~ AppendUnique c1 c2
         ) => P.PSemigroup (PPrototype m v i s x)
             (Many i1, Many s1, Which c1, Which a1, Which b1)
             (Many i2, Many s2, Which c2,  Which a2, Which b2)
             (Many i3, Many s3, Which c3, Which a3, Which b3) where
    (Prototype (dis1, win1, bld1, exec1)) `pmappend` (Prototype (dis2, win2, bld2, exec2)) =
        Prototype
        ( dis1 <> dis2
        , win1 <> win2
        , bld1 `P.pmappend` bld2
        , exec1 `P.pmappend` exec2
        )

------------------------------------------

windowing
    :: Monad m
    => F.Window m (F.ComponentPlan x m, s) ()
    -> Prototype m v i s (Many '[]) (Many '[]) x (Which '[]) (Which '[]) (Which '[])
windowing w = Prototype
        ( mempty
        , w
        , P.pmempty
        , P.pmempty
        )

building
    :: Monad m
    => F.Builder m i s i' s'
    -> Prototype m v i s i' s' x (Which '[]) (Which '[]) (Which '[])
building bld = Prototype
        ( mempty
        , mempty
        , bld
        , P.pmempty
        )

refExecuting
    :: Monad m
    => (F.RefExecutor m v (F.ComponentPlan x m, s) x c a b)
    -> Prototype m v i s (Many '[]) (Many '[]) x c a b
refExecuting exec = Prototype
        ( mempty
        , mempty
        , P.pmempty
        , exec
        )

------------------------------------------

mapWindow
    :: (F.Window m (F.ComponentPlan x m, s) () -> F.Window m (F.ComponentPlan x m, s) ())
    -> Prototype m v i s i' s' x c a b
    -> Prototype m v i s i' s' x c a b
mapWindow f (Prototype (dis, win, bld, exec)) = Prototype
                   ( dis
                   , f win
                   , bld
                   , exec
                   )

mapBuilder
    :: (F.Builder m i1 s i1' s1' -> F.Builder m i2 s i2' s2')
    -> Prototype m v i1 s i1' s1' x c a b
    -> Prototype m v i2 s i2' s2' x c a b
mapBuilder f (Prototype (dis, win, bld, exec)) = Prototype
                   ( dis
                   , win
                   , f bld
                   , exec
                   )

mapRefExecutor
    :: (   (F.RefExecutor m v (F.ComponentPlan x m, s) x c1 a1 b1)
        -> (F.RefExecutor m v (F.ComponentPlan x m, s) x c2 a2 b2))
    -> Prototype m v i s i' s' x c1 a1 b1
    -> Prototype m v i s i' s' x c2 a2 b2
mapRefExecutor f (Prototype (dis, win, bld, exec)) = Prototype
                   ( dis
                   , win
                   , bld
                   , f exec
                   )

------------------------------------------

newtype PrototypeOnModel m v i i' s' x c a b s = PrototypeOnModel
    { runPrototypeOnModel :: Prototype m v i s i' s' x c a b
    }

type instance F.OnModel (PrototypeOnModel m v i i' s' x c a b) s = Prototype m v i s i' s' x c a b

instance F.ViaModel (PrototypeOnModel m v i i' s' x c a b) where
    viaModel l (Prototype (dis, win, bld, exec)) = Prototype
                   ( F.viaModel l dis
                   , F.viaModel (alongside id l) win
                   , F.viaModel l bld
                   , F.viaModel (alongside id l) exec
                   )

------------------------------------------

newtype PrototypeOnInfo m v s i' s' a b x c i = PrototypeOnInfo
    { runPrototypeOnInfo :: Prototype m v i s i' s' a b x c
    }

type instance F.OnInfo (PrototypeOnInfo m v s i' s' a b x c) i = Prototype m v i s i' s' a b x c

instance F.ViaInfo (PrototypeOnInfo m v s i' s' x c a b) where
    viaInfo l (Prototype (dis, win, bld, exec)) = Prototype
                   ( dis
                   , win
                   , F.viaInfo l bld
                   , exec
                   )
