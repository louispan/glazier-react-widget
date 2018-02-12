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

module Glazier.React.Framework.Core.Prototype where

import Control.Lens
-- import Data.Coerce
import Data.Diverse.Profunctor
import Data.Semigroup
import Data.Tagged
import qualified GHC.Generics as G
import qualified GHCJS.Types as J
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Core.Builder as F
import qualified Glazier.React.Framework.Core.Display as F
import qualified Glazier.React.Framework.Core.Executor as F
import qualified Glazier.React.Framework.Core.Finalizer as F
import qualified Glazier.React.Framework.Core.Model as F
import qualified JavaScript.Extras as JE
import qualified Parameterized.Data.Monoid as P
import qualified Parameterized.TypeLevel as P

data Prototype x m v i s i' s' y z a b = Prototype
    { builder :: F.Builder m i s i' s'
    , display :: F.FrameDisplay x m s ()
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
        F.nilBuilder
        mempty
        mempty
        F.nilExActivator
        F.nilExHandler

-- | type restricted version of 'P.pmempty' for 'Prototype'
nilPrototype :: R.MonadReactor x m => Prototype x m v i s
    (Many '[])
    (Many '[])
    (Which '[])
    (Which '[])
    (Which '[])
    (Which '[])
nilPrototype = P.pmempty

-- | A friendlier constraint synonym for 'PBuilder' 'pmappend'.
type PmappendPrototype i1 i2 i3 s1 s2 s3 y1 y2 y3 z1 z2 z3 a1 a2 a3 b1 b2 b3 =
    ( F.PmappendBuilder i1 i2 i3 s1 s2 s3
    , ChooseBetween a1 a2 a3 b1 b2 b3
    , F.PmappendExecutor y1 y2 y3
    , F.PmappendExecutor z1 z2 z3
    )

instance ( R.MonadReactor x m
         , PmappendPrototype i1 i2 i3 s1 s2 s3 y1 y2 y3 z1 z2 z3 a1 a2 a3 b1 b2 b3
         ) => P.PSemigroup (PPrototype x m v i s)
             (Many i1, Many s1, Which y1, Which z1, Which a1, Which b1)
             (Many i2, Many s2, Which y2, Which z2, Which a2, Which b2)
             (Many i3, Many s3, Which y3, Which z3, Which a3, Which b3) where
    (Prototype bld1 dis1 fin1 act1 hdl1) `pmappend` (Prototype bld2 dis2 fin2 act2 hdl2) =
        Prototype
        (bld1 `F.andBuilder` bld2)
        (dis1 <> dis2)
        (fin1 <> fin2)
        (act1 `F.andExActivator` act2)
        (hdl1 `F.andExHandler` hdl2)

-- | type restricted version of 'P.pmappend' for 'Prototype'
andPrototype :: forall x m v i s i1 i2 i3 s1 s2 s3 y1 y2 y3 z1 z2 z3 a1 a2 a3 b1 b2 b3.
    ( R.MonadReactor x m
    , PmappendPrototype i1 i2 i3 s1 s2 s3 y1 y2 y3 z1 z2 z3 a1 a2 a3 b1 b2 b3
    )
    => Prototype x m v i s (Many i1) (Many s1) (Which y1) (Which z1) (Which a1) (Which b1)
    -> Prototype x m v i s (Many i2) (Many s2) (Which y2) (Which z2) (Which a2) (Which b2)
    -> Prototype x m v i s (Many i3) (Many s3) (Which y3) (Which z3) (Which a3) (Which b3)
andPrototype = P.pmappend
infixr 6 `andPrototype` -- like mappend

------------------------------------------

newtype PrototypeOnSpec x m v i i' s' y z a b s = PrototypeOnSpec
    { runPrototypeOnSpec :: Prototype x m v i s i' s' y z a b
    }

type instance F.OnSpec (PrototypeOnSpec x m v i i' s' y z a b) s = Prototype x m v i s i' s' y z a b

instance F.ViaSpec (PrototypeOnSpec x m v i i' s' y z a b) where
    viaSpec l (Prototype bld dis fin act hdl) = Prototype
        (F.viaSpec l bld)
        (F.viaSpec (alongside id l) dis)
        (F.viaSpec l fin)
        (F.viaSpec (alongside id l) act)
        (F.viaSpec (alongside id l) hdl)

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
-- @AllowAmbiguousTypes@: Use @TypeApplications@ instead of @Proxy@ to specify @t@
widget ::
    forall t x m v i s i' ss' y z a b.
    ( Monad m
    , HasItemTag' t [R.Listener] s)
    => J.JSString
    -> (F.Frame x m s -> [JE.Property])
    -> Prototype x m v i s i' (Many ss') y z a b
    -> Prototype x m v i s
        i'
        (Many (Tagged t [R.Listener] ': ss'))
        y z a b
widget n f (Prototype (F.Builder (mkInf, F.MkSpec mkSpc)) dis fin act hdl) =
    Prototype
    (F.Builder ( mkInf
                , F.MkSpec $ \is -> (\s -> (is ^. mempty
                                            ./ s))
                    <$> mkSpc is))
    (\s ->
            let props = f s
                ls = s ^. F.model.itemTag' @t @[R.Listener]
            in R.branch (JE.toJS' n)
                    ls
                    props
                    (dis s))
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
-- widget n ts (Prototype (F.Builder (F.MkInfo mkInf, F.MkSpec mkSpc)) (F.Display dis) fin act hdl) =
--     Prototype
--     (F.Builder ( F.MkInfo $ \ss -> (\i -> (ss ^. item' @[JE.Property]) ./ i)
--                     <$> mkInf ss
--                 , F.MkSpec $ \is -> (\s -> (is ^. item' @[JE.Property])
--                                             ./ mempty
--                                             ./ ts
--                                             ./ s)
--                     <$> mkSpc is))
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
enclose ji ij ts st p =
    let p'@(Prototype bld _ _ _ _) = F.viaSpec ts (F.viaInfo ji p)
    in p' { builder = F.mapBuilder ij st bld }

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

-- | Wrap a prototype's info and model as an item inside a Many.
comprise
    :: ( Functor m
       , HasItem' s1 s2
       , HasItem' i1 i2
       )
    => Prototype x m v i1 s1 i' s' y z a b
    -> Prototype x m v i2 s2 (Many '[i']) (Many '[s']) y z a b
comprise p =
    let p'@(Prototype bld _ _ _ _) = F.viaSpec item' (F.viaInfo item' p)
    in p' { builder = F.mapBuilder single single bld }
