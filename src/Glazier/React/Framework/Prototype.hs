{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Framework.Prototype where

import Control.Lens
import Data.Coerce
import Data.Diverse.Profunctor
import qualified Data.DList as DL
import Data.Semigroup
import qualified GHCJS.Types as J
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Builder as F
import qualified Glazier.React.Framework.Core as F
import qualified Glazier.React.Framework.Display as F
import qualified Glazier.React.Framework.Executor as F
import qualified Glazier.React.Framework.Finalizer as F
import qualified JavaScript.Extras as JE
import qualified Parameterized.Data.Monoid as P
import qualified Parameterized.TypeLevel as P

newtype Prototype m v i s i' s' x y z a b = Prototype {
    runPrototype ::
           ( F.Finalizer m s
           , F.Display m (F.ComponentPlan x m, s) ()
           , F.Builder m i s i' s'
           , F.ExObjActivator m v (F.ComponentPlan x m, s) x y
           , F.ExObjHandler m v (F.ComponentPlan x m, s) x z a b
           )
    }

------------------------------------------

newtype PPrototype m v i s x isyzab = PPrototype {
    runPPrototype :: Prototype m v i s (P.At0 isyzab) (P.At1 isyzab) x (P.At2 isyzab) (P.At3 isyzab) (P.At4 isyzab) (P.At5 isyzab)
    }

type instance P.PNullary (PPrototype m v i s x) (i', s', y, z, a, b) = Prototype m v i s i' s' x y z a b

instance R.MonadReactor x m => P.PMEmpty (PPrototype m i s v x) (Many '[], Many '[], Which '[], Which '[], Which '[], Which '[]) where
    pmempty = Prototype
        ( mempty
        , mempty
        , P.pmempty
        , P.pmempty
        , P.pmempty
        )

instance ( R.MonadReactor x m
         , i3 ~ Append i1 i2
         , s3 ~ Append s1 s2
         , ChooseBetween a1 a2 a3 b1 b2 b3
         , F.PmappendExecutor y1 y2 y3
         , F.PmappendExecutor z1 z2 z3
         ) => P.PSemigroup (PPrototype m v i s x)
             (Many i1, Many s1, Which y1, Which z1, Which a1, Which b1)
             (Many i2, Many s2, Which y2, Which z2, Which a2, Which b2)
             (Many i3, Many s3, Which y3, Which z3, Which a3, Which b3) where
    (Prototype (fin1, dis1, bld1, act1, hdl1)) `pmappend` (Prototype (fin2, dis2, bld2, act2, hdl2)) =
        Prototype
        ( fin1 <> fin2
        , dis1 <> dis2
        , bld1 `P.pmappend` bld2
        , act1 `P.pmappend` act2
        , hdl1 `P.pmappend` hdl2
        )

------------------------------------------

displaying
    :: Monad m
    => F.Display m (F.ComponentPlan x m, s) ()
    -> Prototype m v i s (Many '[]) (Many '[]) x (Which '[]) (Which '[]) (Which '[]) (Which '[])
displaying w = Prototype
        ( mempty
        , w
        , P.pmempty
        , P.pmempty
        , P.pmempty
        )

building
    :: Monad m
    => F.Builder m i s i' s'
    -> Prototype m v i s i' s' x (Which '[]) (Which '[]) (Which '[]) (Which '[])
building bld = Prototype
        ( mempty
        , mempty
        , bld
        , P.pmempty
        , P.pmempty
        )

exObjActivating
    :: Monad m
    => F.ExObjActivator m v (F.ComponentPlan x m, s) x y
    -> Prototype m v i s (Many '[]) (Many '[]) x y (Which '[]) (Which '[]) (Which '[])
exObjActivating act = Prototype
        ( mempty
        , mempty
        , P.pmempty
        , act
        , P.pmempty
        )

exObjHandling
    :: Monad m
    => F.ExObjHandler m v (F.ComponentPlan x m, s) x z a b
    -> Prototype m v i s (Many '[]) (Many '[]) x (Which '[]) z a b
exObjHandling hdl = Prototype
        ( mempty
        , mempty
        , P.pmempty
        , P.pmempty
        , hdl
        )
------------------------------------------

mapDisplay
    :: (F.Display m (F.ComponentPlan x m, s) () -> F.Display m (F.ComponentPlan x m, s) ())
    -> Prototype m v i s i' s' x y z a b
    -> Prototype m v i s i' s' x y z a b
mapDisplay f (Prototype (fin, dis, bld, act, hdl)) = Prototype
                   ( fin
                   , f dis
                   , bld
                   , act
                   , hdl
                   )

mapBuilder
    :: (F.Builder m i1 s i1' s1' -> F.Builder m i2 s i2' s2')
    -> Prototype m v i1 s i1' s1' x y z a b
    -> Prototype m v i2 s i2' s2' x y z a b
mapBuilder f (Prototype (fin, dis, bld, act, hdl)) = Prototype
                   ( fin
                   , dis
                   , f bld
                   , act
                   , hdl
                   )

mapExObjActivator
    :: (   F.ExObjActivator m v (F.ComponentPlan x m, s) x y1
        -> F.ExObjActivator m v (F.ComponentPlan x m, s) x y2)
    -> Prototype m v i s i' s' x y1 z a b
    -> Prototype m v i s i' s' x y2 z a b
mapExObjActivator f (Prototype (fin, dis, bld, act, hdl)) = Prototype
                   ( fin
                   , dis
                   , bld
                   , f act
                   , hdl
                   )

mapExObjHandler
    :: (   F.ExObjHandler m v (F.ComponentPlan x m, s) x z1 a1 b1
        -> F.ExObjHandler m v (F.ComponentPlan x m, s) x z2 a2 b2)
    -> Prototype m v i s i' s' x y z1 a1 b1
    -> Prototype m v i s i' s' x y z2 a2 b2
mapExObjHandler f (Prototype (fin, dis, bld, act, hdl)) = Prototype
                   ( fin
                   , dis
                   , bld
                   , act
                   , f hdl
                   )

------------------------------------------

newtype PrototypeOnModel m v i i' s' x y z a b s = PrototypeOnModel
    { runPrototypeOnModel :: Prototype m v i s i' s' x y z a b
    }

type instance F.OnModel (PrototypeOnModel m v i i' s' x y z a b) s = Prototype m v i s i' s' x y z a b

instance F.ViaModel (PrototypeOnModel m v i i' s' x y z a b) where
    viaModel l (Prototype (fin, dis, bld, act, hdl)) = Prototype
                   ( F.viaModel l fin
                   , F.viaModel (alongside id l) dis
                   , F.viaModel l bld
                   , F.viaModel (alongside id l) act
                   , F.viaModel (alongside id l) hdl
                   )

------------------------------------------

newtype PrototypeOnInfo m v s i' s' a b x y z i = PrototypeOnInfo
    { runPrototypeOnInfo :: Prototype m v i s i' s' x y z a b
    }

type instance F.OnInfo (PrototypeOnInfo m v s i' s' a b x y z) i = Prototype m v i s i' s' x y z a b

instance F.ViaInfo (PrototypeOnInfo m v s i' s' x y z a b) where
    viaInfo l (Prototype (fin, dis, bld, act, hdl)) = Prototype
                   ( fin
                   , dis
                   , F.viaInfo l bld
                   , act
                   , hdl
                   )


-- | Wrap the display of prototype inside a provided 'name', and adds the ability to build
-- @DL.DList JE.Property@ to the info and model, and to build
-- @DL.DList R.Listener@, and @DL.DList F.Trait@ to only the model.
widget ::
    ( Monad m
    , HasItem' (DL.DList JE.Property) is
    , HasItem' (DL.DList JE.Property) ss
    , HasItem' (DL.DList F.Trait) ss
    , HasItem' (DL.DList R.Listener) ss
    )
    => J.JSString
    -> Prototype m v is ss (Many is') (Many ss') x y z a b
    -> Prototype m v is ss
        (Many ((DL.DList JE.Property) ': is'))
        (Many ((DL.DList R.Listener) ': (DL.DList F.Trait) ': (DL.DList JE.Property) ': ss'))
        x y z a b
widget n (Prototype (fin, F.Display dis, F.Builder (F.MkInfo mkInf, F.MkModel mkMdl), act, hdl)) =
    Prototype
        ( fin
        , F.Display $ \(cp, ss) ->
                let props = view (item' @(DL.DList JE.Property)) ss
                    hs = view (item' @(DL.DList F.Trait)) ss
                    ls = view (item' @(DL.DList R.Listener)) ss
                in R.branch (JE.toJS' n)
                        (DL.toList ls)
                        (DL.toList (coerce hs <> props))
                        (dis (cp, ss))
        , F.Builder ( F.MkInfo $ \ss -> (\i -> (ss ^. item' @(DL.DList JE.Property)) ./ i)
                        <$> mkInf ss
                    , F.MkModel $ \is -> (\s -> mempty
                                             ./ mempty
                                             ./ (is ^. item' @(DL.DList JE.Property))
                                             ./ s)
                        <$> mkMdl is
                    )
        , act
        , hdl
        )


-- | Wrap a prototype's info and model as an item inside a Many.
contains
    :: ( Functor m
       , HasItem' s ss
       , HasItem' i is
       )
    => Prototype m v i s i' s' x y z a b
    -> Prototype m v is ss (Many '[i']) (Many '[s']) x y z a b
contains p = mapBuilder (bimap single single) (F.viaModel item' (F.viaInfo item' p))
