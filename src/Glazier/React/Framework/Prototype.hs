{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Framework.Prototype where

import Control.Lens
import Data.Coerce
import Data.Diverse.Profunctor
import Data.Semigroup
import qualified GHC.Generics as G
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

data Prototype m v i s i' s' x y z a b = Prototype
    { builder :: F.Builder m i s i' s'
    , display :: F.Display m (F.ComponentPlan x m, s) ()
    , finalizer :: F.Finalizer m s
    , activator :: F.ExObjActivator m v (F.ComponentPlan x m, s) x y
    , handler :: F.ExObjHandler m v (F.ComponentPlan x m, s) x z a b
    } deriving (G.Generic)

------------------------------------------

newtype PPrototype m v i s x isyzab = PPrototype {
    runPPrototype :: Prototype m v i s (P.At0 isyzab) (P.At1 isyzab) x (P.At2 isyzab) (P.At3 isyzab) (P.At4 isyzab) (P.At5 isyzab)
    }

type instance P.PNullary (PPrototype m v i s x) (i', s', y, z, a, b) = Prototype m v i s i' s' x y z a b

instance R.MonadReactor x m => P.PMEmpty (PPrototype m i s v x) (Many '[], Many '[], Which '[], Which '[], Which '[], Which '[]) where
    pmempty = Prototype
        P.pmempty
        mempty
        mempty
        P.pmempty
        P.pmempty

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
    (Prototype bld1 dis1 fin1 act1 hdl1) `pmappend` (Prototype bld2 dis2 fin2 act2 hdl2) =
        Prototype
        (bld1 `P.pmappend` bld2)
        (dis1 <> dis2)
        (fin1 <> fin2)
        (act1 `P.pmappend` act2)
        (hdl1 `P.pmappend` hdl2)

------------------------------------------

newtype PrototypeOnModel m v i i' s' x y z a b s = PrototypeOnModel
    { runPrototypeOnModel :: Prototype m v i s i' s' x y z a b
    }

type instance F.OnModel (PrototypeOnModel m v i i' s' x y z a b) s = Prototype m v i s i' s' x y z a b

instance F.ViaModel (PrototypeOnModel m v i i' s' x y z a b) where
    viaModel l (Prototype bld dis fin act hdl) = Prototype
        (F.viaModel l bld)
        (F.viaModel (alongside id l) dis)
        (F.viaModel l fin)
        (F.viaModel (alongside id l) act)
        (F.viaModel (alongside id l) hdl)

------------------------------------------

newtype PrototypeOnInfo m v s i' s' a b x y z i = PrototypeOnInfo
    { runPrototypeOnInfo :: Prototype m v i s i' s' x y z a b
    }

type instance F.OnInfo (PrototypeOnInfo m v s i' s' a b x y z) i = Prototype m v i s i' s' x y z a b

instance F.ViaInfo (PrototypeOnInfo m v s i' s' x y z a b) where
    viaInfo l (Prototype bld dis fin act hdl) = Prototype
        (F.viaInfo l bld)
        dis
        fin
        act
        hdl


-- | Wrap the display of prototype inside a provided 'name', and adds the ability to build
-- @[JE.Property]@ to the info and model, and to build
-- @[R.Listener]@, and @DL.DList F.Trait@ to only the model.
widget ::
    ( Monad m
    , HasItem' [JE.Property] i
    , HasItem' [JE.Property] s
    , HasItem' [F.Trait] s
    , HasItem' [R.Listener] s
    )
    => J.JSString
    -> [F.Trait]
    -> Prototype m v i s (Many is') (Many ss') x y z a b
    -> Prototype m v i s
        (Many ([JE.Property] ': is'))
        (Many ([R.Listener] ': [F.Trait] ': [JE.Property] ': ss'))
        x y z a b
widget n ts (Prototype (F.Builder (F.MkInfo mkInf, F.MkModel mkMdl)) (F.Display dis) fin act hdl) =
    Prototype
    (F.Builder ( F.MkInfo $ \ss -> (\i -> (ss ^. item' @[JE.Property]) ./ i)
                    <$> mkInf ss
                , F.MkModel $ \is -> (\s -> mempty
                                            ./ ts
                                            ./ (is ^. item' @[JE.Property])
                                            ./ s)
                    <$> mkMdl is))
    (F.Display $ \(cp, ss) ->
            let props = view (item' @[JE.Property]) ss
                hs = view (item' @[F.Trait]) ss
                ls = view (item' @[R.Listener]) ss
            in R.branch (JE.toJS' n)
                    ls
                    (coerce hs <> props)
                    (dis (cp, ss)))
    fin
    act
    hdl

-- | Wrap a prototype's info and model as an item inside a Many.
-- enclose
--     :: ( Functor m
--        , HasItem' s1 s2
--        , HasItem' i1 i2
--        )
--     => Prototype m v i1 s1 i' s' x y z a b
--     -> Prototype m v i2 s2 (Many '[i']) (Many '[s']) x y z a b
-- enclose p = mapBuilder (bimap single single) (F.viaModel item' (F.viaInfo item' p))

-- -- | Wrap a prototype's info and model as an item inside a Tagged and then a Many.
-- entagged
--     :: forall t m v i1 i2 s1 s2 i' s' x y z a b.
--         ( Functor m
--         , HasItem' (Tagged t s1) s2
--         , HasItem' (Tagged t i1) i2
--         )
--     => Prototype m v i1 s1 i' s' x y z a b
--     -> Prototype m v
--         i2 s2
--         (Many '[Tagged t i']) (Many '[Tagged t s'])
--         x y z a b
-- entagged p = enclose (F.viaModel (from _Wrapped') (mapBuilder (F.dimapInfo unTagged (Tagged @t)) p))
