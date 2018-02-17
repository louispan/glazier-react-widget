{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
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
import Data.Diverse.Profunctor
import Data.Semigroup
import Data.Tagged
import qualified GHC.Generics as G
import qualified GHCJS.Types as J
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Core.Activator as F
import qualified Glazier.React.Framework.Core.Builder as F
import qualified Glazier.React.Framework.Core.Display as F
import qualified Glazier.React.Framework.Core.Finalizer as F
import qualified Glazier.React.Framework.Core.Handler as F
import qualified Glazier.React.Framework.Core.Model as F
import qualified JavaScript.Extras as JE
import qualified Parameterized.Data.Monoid as P
import qualified Parameterized.TypeLevel as P

data Prototype m v i s i' s' c a b = Prototype
    { builder' :: F.Builder m i s i' s'
    , display' :: F.FrameDisplay m s ()
    , finalizer' :: F.Finalizer m s
    , activator' :: F.SceneActivator m v s c
    , handler' :: F.SceneHandler m v s a b
    } deriving (G.Generic)

------------------------------------------

newtype PPrototype m v i s iscab = PPrototype {
    runPPrototype :: Prototype m v i s (P.At0 iscab) (P.At1 iscab) (P.At2 iscab) (P.At3 iscab) (P.At4 iscab)
    }

type instance P.PNullary (PPrototype m v i s) (i', s', c, a, b) = Prototype m v i s i' s' c a b

instance R.MonadReactor m => P.PMEmpty (PPrototype m i s v) (Many '[], Many '[], Which '[], Which '[], Which '[]) where
    pmempty = Prototype
        F.nulBuilder
        mempty
        F.nulFinalizer
        F.nulActivator
        F.nulHandler

-- | type restricted version of 'P.pmempty' for 'Prototype'
nulPrototype :: R.MonadReactor m => Prototype m v i s
    (Many '[])
    (Many '[])
    (Which '[])
    (Which '[])
    (Which '[])
nulPrototype = P.pmempty

-- | A friendlier constraint synonym for 'PBuilder' 'pmappend'.
type AndPrototype i1 i2 i3 s1 s2 s3 c1 c2 c3 a1 a2 a3 b1 b2 b3 =
    ( F.AndBuilder i1 i2 i3 s1 s2 s3
    , ChooseBoth c1 c2 c3
    , ChooseBetween a1 a2 a3 b1 b2 b3
    )

instance ( R.MonadReactor m
         , AndPrototype i1 i2 i3 s1 s2 s3 c1 c2 c3 a1 a2 a3 b1 b2 b3
         ) => P.PSemigroup (PPrototype m v i s)
             (Many i1, Many s1, Which c1, Which a1, Which b1)
             (Many i2, Many s2, Which c2, Which a2, Which b2)
             (Many i3, Many s3, Which c3, Which a3, Which b3) where
    (Prototype bld1 dis1 fin1 act1 hdl1) `pmappend` (Prototype bld2 dis2 fin2 act2 hdl2) =
        Prototype
        (bld1 `F.andBuilder` bld2)
        (dis1 <> dis2)
        (fin1 `F.andFinalizer` fin2)
        (act1 `F.andActivator` act2)
        (hdl1 `F.orHandler` hdl2)

-- | type restricted version of 'P.pmappend' for 'Prototype'
andPrototype :: forall m v i s i1 i2 i3 s1 s2 s3 c1 c2 c3 a1 a2 a3 b1 b2 b3.
    ( R.MonadReactor m
    , AndPrototype i1 i2 i3 s1 s2 s3 c1 c2 c3 a1 a2 a3 b1 b2 b3
    )
    => Prototype m v i s (Many i1) (Many s1) (Which c1) (Which a1) (Which b1)
    -> Prototype m v i s (Many i2) (Many s2) (Which c2) (Which a2) (Which b2)
    -> Prototype m v i s (Many i3) (Many s3) (Which c3) (Which a3) (Which b3)
andPrototype = P.pmappend
infixr 6 `andPrototype` -- like mappend

------------------------------------------

newtype PrototypeOnSpec m v i i' s' c a b s = PrototypeOnSpec
    { runPrototypeOnSpec :: Prototype m v i s i' s' c a b
    }

instance F.ViaSpec (PrototypeOnSpec m v i i' s' c a b) where
    type OnSpec (PrototypeOnSpec m v i i' s' c a b) s = Prototype m v i s i' s' c a b
    viaSpec l (Prototype bld dis fin act hdl) = Prototype
        (F.viaSpec l bld)
        (F.viaSpec (alongside id l) dis)
        (F.viaSpec l fin)
        (F.viaObj (alongside id l) act)
        (F.viaObj (alongside id l) hdl)

------------------------------------------

newtype PrototypeOnInfo m v s i' s' c a b i = PrototypeOnInfo
    { runPrototypeOnInfo :: Prototype m v i s i' s' c a b
    }

instance F.ViaInfo (PrototypeOnInfo m v s i' s' c a b) where
    type OnInfo (PrototypeOnInfo m v s i' s' c a b) i = Prototype m v i s i' s' c a b
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
    forall t m v i s i' ss' c a b.
    ( Monad m
    , HasItemTag' t [R.Listener] s)
    => J.JSString
    -> (F.Frame m s -> [JE.Property])
    -> Prototype m v i s i' (Many ss') c a b
    -> Prototype m v i s
        i'
        (Many (Tagged t [R.Listener] ': ss'))
        c a b
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
    -> Prototype m v i s i' s' a x y
    -> Prototype m v j t j' t' a x y
enclose ji ij ts st p =
    let p'@(Prototype bld _ _ _ _) = F.viaSpec ts (F.viaInfo ji p)
    in p' { builder' = F.mapBuilder ij st bld }

encloseTagged :: forall t m v i s i' s' a x y.
    Functor m
    => Prototype m v i s i' s' a x y
    -> Prototype m v (Tagged t i) (Tagged t s) (Tagged t i') (Tagged t s') a x y
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
    => Prototype m v i1 s1 i' s' a x y
    -> Prototype m v i2 s2 (Many '[i']) (Many '[s']) a x y
comprise p =
    let p'@(Prototype bld _ _ _ _) = F.viaSpec item' (F.viaInfo item' p)
    in p' { builder' = F.mapBuilder single single bld }
