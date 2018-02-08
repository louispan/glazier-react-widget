-- {-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Framework.Prototype where

import Control.Lens
-- import Data.Coerce
import Data.Diverse.Profunctor
import Data.Semigroup
import Data.Tagged
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

data Prototype x m v i s i' s' y z a b = Prototype
    { builder :: F.Builder m i s i' s'
    , display :: F.ComDisplay x m s ()
    , finalizer :: F.Finalizer m s
    , activator :: F.ProtoActivator x m v s y
    , handler :: F.ProtoHandler x m v s z a b
    } deriving (G.Generic)

------------------------------------------

newtype PPrototype x m v i s isyzab = PPrototype {
    runPPrototype :: Prototype x m v i s (P.At0 isyzab) (P.At1 isyzab) (P.At2 isyzab) (P.At3 isyzab) (P.At4 isyzab) (P.At5 isyzab)
    }

type instance P.PNullary (PPrototype x m v i s) (i', s', y, z, a, b) = Prototype x m v i s i' s' y z a b

instance R.MonadReactor x m => P.PMEmpty (PPrototype x m i s v) (Many '[], Many '[], Which '[], Which '[], Which '[], Which '[]) where
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
         ) => P.PSemigroup (PPrototype x m v i s)
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

newtype PrototypeOnModel x m v i i' s' y z a b s = PrototypeOnModel
    { runPrototypeOnModel :: Prototype x m v i s i' s' y z a b
    }

type instance F.OnModel (PrototypeOnModel x m v i i' s' y z a b) s = Prototype x m v i s i' s' y z a b

instance F.ViaModel (PrototypeOnModel x m v i i' s' y z a b) where
    viaModel l (Prototype bld dis fin act hdl) = Prototype
        (F.viaModel l bld)
        (F.viaModel (alongside id l) dis)
        (F.viaModel l fin)
        (F.viaModel (alongside id l) act)
        (F.viaModel (alongside id l) hdl)

------------------------------------------

newtype PrototypeOnInfo x m v s i' s' a b y z i = PrototypeOnInfo
    { runPrototypeOnInfo :: Prototype x m v i s i' s' y z a b
    }

type instance F.OnInfo (PrototypeOnInfo x m v s i' s' a b y z) i = Prototype x m v i s i' s' y z a b

instance F.ViaInfo (PrototypeOnInfo x m v s i' s' y z a b) where
    viaInfo l (Prototype bld dis fin act hdl) = Prototype
        (F.viaInfo l bld)
        dis
        fin
        act
        hdl

-- type WidgetInfo is = Many ([JE.Property] ': is)
-- type WidgetModel ss = Many ([JE.Property] ': [R.Listener] ': [F.Trait] ': ss)
-- type WidgetModel t ss = Many (Tagged t [R.Listener] ': ss)
-- type WidgetModel' ss = Many ([R.Listener] ': [F.Trait] ': ss)
-- type IsWidget i s =
--     ( HasItem' [R.Listener] s)

-- | Wrap the display of prototype inside a provided 'name', and adds the ability to build
-- @[R.Listener]@ to the model.
widget ::
    forall t x m v i s i' ss' y z a b.
    ( Monad m
    , HasItemTag' t [R.Listener] s)
    => J.JSString
    -> (s -> [JE.Property])
    -> Prototype x m v i s i' (Many ss') y z a b
    -> Prototype x m v i s
        i'
        (Many (Tagged t [R.Listener] ': ss'))
        y z a b
widget n f (Prototype (F.Builder (mkInf, F.MkModel mkMdl)) (F.Display dis) fin act hdl) =
    Prototype
    (F.Builder ( mkInf
                , F.MkModel $ \is -> (\s -> (is ^. mempty
                                            ./ s))
                    <$> mkMdl is))
    (F.Display $ \(cp, ss) ->
            let props = f ss
                ls = view (itemTag' @t @[R.Listener]) ss
            in R.branch (JE.toJS' n)
                    ls
                    props
                    (dis (cp, ss)))
    fin
    act
    hdl

-- -- | Wrap the display of prototype inside a provided 'name', and adds the ability to build
-- -- @[JE.Property]@ to the info and model, and to build
-- -- @[R.Listener]@, and @DL.DList F.Trait@ to only the model.
-- widget ::
--     ( Monad m
--     , IsWidget i s)
--     => J.JSString
--     -> (Many ss' -> [F.Trait])
--     -> Prototype m v i s i' (Many ss') x y z a b
--     -> Prototype m v i s
--         i'
--         (Many ([R.Listener] ': ss'))
--         x y z a b
-- widget n ts (Prototype (F.Builder (F.MkInfo mkInf, F.MkModel mkMdl)) (F.Display dis) fin act hdl) =
--     Prototype
--     (F.Builder ( F.MkInfo $ \ss -> (\i -> (ss ^. item' @[JE.Property]) ./ i)
--                     <$> mkInf ss
--                 , F.MkModel $ \is -> (\s -> (is ^. item' @[JE.Property])
--                                             ./ mempty
--                                             ./ ts
--                                             ./ s)
--                     <$> mkMdl is))
--     (F.Display $ \(cp, ss) ->
--             let props = view (item' @[JE.Property]) ss
--                 hs = view (item' @[F.Trait]) ss
--                 ls = view (item' @[R.Listener]) ss
--             in R.branch (JE.toJS' n)
--                     ls
--                     (coerce hs <> props)
--                     (dis (cp, ss)))
--     fin
--     act
--     hdl

-- | Apply isomorphisms of Info and Model to the prototype
enclose :: Functor m
    => Iso' j i
    -> (i' -> j')
    -> Iso' t s
    -> (s' -> t')
    -> Prototype x m v i s i' s' y z a b
    -> Prototype x m v j t j' t' y z a b
enclose ji ij ts st (Prototype bld dis fin act hdl) = Prototype
    (F.mapBuilder (view ji) ij (view ts) st bld)
    (F.viaModel (alongside id ts) dis)
    (F.viaModel ts fin)
    (F.viaModel (alongside id ts) act)
    (F.viaModel (alongside id ts) hdl)

encloseTagged :: forall t x m v i s i' s' y z a b.
    Functor m
    => Prototype x m v i s i' s' y z a b
    -> Prototype x m v (Tagged t i) (Tagged t s) (Tagged t i') (Tagged t s') y z a b
encloseTagged p =
    let ts :: Iso' (Tagged t s) s
        ts = iso unTagged Tagged
        ji :: Iso' (Tagged t i) i
        ji = iso unTagged Tagged
    in enclose ji (Tagged @t) ts (Tagged @t) p

-- | Wrap a prototype's info and model as an item inside a Manysans.
comprise
    :: ( Functor m
       , HasItem' s1 s2
       , HasItem' i1 i2
       )
    => Prototype x m v i1 s1 i' s' y z a b
    -> Prototype x m v i2 s2 (Many '[i']) (Many '[s']) y z a b
comprise p =
    let p'@(Prototype bld _ _ _ _) = F.viaModel item' (F.viaInfo item' p)
    in p' { builder = F.mapBuilder id single id single bld }
