{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Framework.Gizmo where

import Control.Applicative
import Control.Lens
import Control.Monad.Free.Church
import Data.Diverse.Lens
import Data.Kind
import Data.Proxy
import Data.Semigroup
import qualified Data.DList as D
import qualified Glazier as G
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Widget as F

type MkPlan plns acts = F (R.Maker (Which acts)) (Many plns)
type MkDetail dtls ols acts = Many ols -> F (R.Maker (Which acts)) (Many dtls)
type FromDetail ols dtls = Many dtls -> Many ols
type Gadgetry dtls plns acts cmds = G.Gadget (Which acts) (F.Entity dtls plns) (D.DList (Which cmds))

newtype Gizmo (o :: [Type]) ols (d :: [Type]) dtls (p :: [Type]) plns (a :: [Type]) acts (c :: [Type]) cmds = Gizmo (Proxy a, Proxy c, MkDetail d ols acts, FromDetail o dtls, MkPlan p acts, Gadgetry dtls plns acts cmds)

andGizmo :: Gizmo o1 ols d1 dtls p1 plns a1 acts c1 cmds
         -> Gizmo o2 ols d2 dtls p2 plns a2 acts c2 cmds
         -> Gizmo (Append o1 o2) ols (Append d1 d2) dtls (Append p1 p2) plns (AppendUnique a1 a2) acts (AppendUnique c1 c2) cmds
andGizmo (Gizmo (_, _, mkDtl, fromDtl, mkPln, dev)) (Gizmo (_, _, mkDtl', fromDtl', mkPln', dev')) =
        Gizmo ( Proxy
              , Proxy
              , \o -> (/./) <$> mkDtl o <*> mkDtl' o
              , \d -> fromDtl d /./ fromDtl' d
              , (/./) <$> mkPln <*> mkPln'
              , dev <> dev')

orGizmo :: Gizmo o1 ols d1 dtls p1 plns a1 acts c1 cmds
        -> Gizmo o2 ols d2 dtls p2 plns a2 acts c2 cmds
        -> Gizmo (Append o1 o2) ols (Append d1 d2) dtls (Append p1 p2) plns (AppendUnique a1 a2) acts (AppendUnique c1 c2) cmds
orGizmo (Gizmo (_, _, mkDtl, fromDtl, mkPln, dev)) (Gizmo (_, _, mkDtl', fromDtl', mkPln', dev')) =
        Gizmo ( Proxy
              , Proxy
              , \o -> (/./) <$> mkDtl o <*> mkDtl' o
              , \d -> fromDtl d /./ fromDtl' d
              , (/./) <$> mkPln <*> mkPln'
              , dev <|> dev')

-- | Identify for 'orGizmo' or 'andGizmo'
noop :: Gizmo '[] ols '[] dtls '[] plns '[] acts '[] cmds
noop = Gizmo ( Proxy
             , Proxy
             , const $ pure nil
             , const nil
             , pure nil
             , empty)

-- | lift a 'Gadgetry' into a 'Gizmo'
gizmo :: (Proxy a, Proxy c, Gadgetry dtls plns acts cmds) -> Gizmo '[] ols '[] dtls '[] plns a acts c cmds
gizmo (_, _, g) = Gizmo ( Proxy
                , Proxy
                , const $ pure nil
                , const nil
                , pure nil
                , g)

gadgetry
    :: forall a c dtls plns acts cmds.
       (UniqueMember a acts, UniqueMember c cmds)
    => G.Gadget a (F.Entity dtls plns) (D.DList c)
    -> (Proxy '[a], Proxy '[c], Gadgetry dtls plns acts cmds)
gadgetry g = (Proxy, Proxy, magnify (facet @a) (fmap (pick @_ @c) <$> g))
