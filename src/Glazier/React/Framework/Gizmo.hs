{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Framework.Gizmo where

import Control.Applicative
import Control.Lens
import Control.Monad.Free.Church
import Data.Diverse.Lens
import Data.Semigroup
import qualified Data.DList as D
import qualified Glazier as G
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Widget as F
import qualified Glazier.React.Framework.Attach as F

type MkPlan plns acts = F (R.Maker (Which acts)) (Many plns)
type MkDetail dtls ols acts = Many ols -> F (R.Maker (Which acts)) (Many dtls)
type FromDetail ols dtls = Many dtls -> Many ols
type Gadgetry dtls plns acts cmds = G.Gadget (Which acts) (F.Entity dtls plns) (D.DList (Which cmds))

newtype Gizmo o ols d dtls p plns acts cmds = Gizmo (MkDetail d ols acts, FromDetail o dtls, MkPlan p acts, Gadgetry dtls plns acts cmds)

instance (o3 ~ Append o1 o2, d3 ~ Append d1 d2, p3 ~ Append p1 p2) =>
         F.Attach (Gizmo o1 ols d1 dtls p1 plns acts cmds)
                (Gizmo o2 ols d2 dtls p2 plns acts cmds)
                (Gizmo o3 ols d3 dtls p3 plns acts cmds) where
    Gizmo (mkDtl, fromDtl, mkPln, dev) +<>+ Gizmo (mkDtl', fromDtl', mkPln', dev') =
        Gizmo ( \o -> (/./) <$> mkDtl o <*> mkDtl' o
              , \d -> fromDtl d /./ fromDtl' d
              , (/./) <$> mkPln <*> mkPln'
              , dev <> dev')
    Gizmo (mkDtl, fromDtl, mkPln, dev) +<|>+ Gizmo (mkDtl', fromDtl', mkPln', dev') =
        Gizmo ( \o -> (/./) <$> mkDtl o <*> mkDtl' o
              , \d -> fromDtl d /./ fromDtl' d
              , (/./) <$> mkPln <*> mkPln'
              , dev <|> dev')

instance F.AttachId (Gizmo '[] ols '[] dtls '[] plns acts cmds) where
    aempty = noop

-- | Identify for 'F.Attach'
noop :: Gizmo '[] ols '[] dtls '[] plns acts cmds
noop = Gizmo ( const $ pure nil
          , const nil
          , pure nil
          , empty)

-- | lift a 'Gadgetry' into a 'Gizmo'
gizmo :: Gadgetry dtls plns acts cmds -> Gizmo '[] ols '[] dtls '[] plns acts cmds
gizmo dev = Gizmo ( const $ pure nil
          , const nil
          , pure nil
          , dev)

gadgetry
    :: forall a c dtls plns acts cmds.
       (UniqueMember a acts, UniqueMember c cmds)
    => G.Gadget a (F.Entity dtls plns) (D.DList c)
    -> G.Gadget (Which acts) (F.Entity dtls plns) (D.DList (Which cmds))
gadgetry g = magnify (facet @a) (fmap (pick @_ @c) <$> g)
