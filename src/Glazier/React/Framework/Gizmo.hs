{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Framework.Gizmo where

import Control.Applicative
import Control.Monad.Free.Church
import Data.Diverse.Lens
import qualified Data.DList as D
import Data.Semigroup
import qualified Glazier as G
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Core as F
import qualified Glazier.React.Framework.Attach as F

type MkPlan plns acts = F (R.Maker (Which acts)) (Many plns)
type MkDetail dtls ols acts = Many ols -> F (R.Maker (Which acts)) (Many dtls)
type ToOutline ols dtls = Many dtls -> Many ols
type Device dtls plns v acts cmds = G.Gadget (Which acts) (F.Entity dtls plns v) (D.DList (Which cmds))

newtype Gizmo o d p ols dtls plns v acts cmds = Gizmo
    { runGizmo :: (MkDetail d ols acts, ToOutline o dtls, MkPlan p acts, Device dtls plns v acts cmds)
    }

instance (o3 ~ Append o1 o2, d3 ~ Append d1 d2, p3 ~ Append p1 p2) =>
         F.Attach (Gizmo o1 d1 p1 ols dtls plns v acts cmds)
                (Gizmo o2 d2 p2 ols dtls plns v acts cmds)
                (Gizmo o3 d3 p3 ols dtls plns v acts cmds) where
    Gizmo (mkDtl, toOl, mkPln, dev) +<>+ Gizmo (mkDtl', toOl', mkPln', dev') =
        Gizmo ( \o -> (/./) <$> mkDtl o <*> mkDtl' o
              , \d -> toOl d /./ toOl' d
              , (/./) <$> mkPln <*> mkPln'
              , dev <> dev')
    Gizmo (mkDtl, toOl, mkPln, dev) +<|>+ Gizmo (mkDtl', toOl', mkPln', dev') =
        Gizmo ( \o -> (/./) <$> mkDtl o <*> mkDtl' o
              , \d -> toOl d /./ toOl' d
              , (/./) <$> mkPln <*> mkPln'
              , dev <|> dev')

instance F.AttachId (Gizmo '[] '[] '[] ols dtls plns v acts cmds) where
    aempty = noop

-- | Identify for 'F.Attach'
noop :: Gizmo '[] '[] '[] ols dtls plns v acts cmds
noop = Gizmo ( const $ pure nil
          , const nil
          , pure nil
          , empty)
