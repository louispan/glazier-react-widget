{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Framework.Prototype where

import Control.Applicative
import Data.Diverse
import Data.Kind
import Data.Proxy
import Data.Semigroup
import qualified Glazier.React.Framework.Create as F
import qualified Glazier.React.Framework.Display as F
import qualified Glazier.React.Framework.Firsts as F
import qualified Glazier.React.Framework.Gadgetry as F
import qualified Glazier.React.Framework.Trigger as F

newtype Prototype (o :: [Type]) ols (d :: [Type]) dtls (p :: [Type]) plns (a :: [Type]) acts (c :: [Type]) cmds =
    Prototype ( F.Display dtls plns
              , F.Gadgetry dtls plns a acts c cmds
              , F.Trigger a acts
              , F.Create o ols d dtls p plns acts)

-- | The action and command types are merged, not appended
andPrototype
    :: Prototype o1 ols d1 dtls p1 plns a1 acts c1 cmds
    -> Prototype o2 ols d2 dtls p2 plns a2 acts c2 cmds
    -> Prototype (Append o1 o2) ols (Append d1 d2) dtls (Append p1 p2) plns (AppendUnique a1 a2) acts (AppendUnique c1 c2) cmds
andPrototype (Prototype (d, g, t, f)) (Prototype (d', g', t', f')) =
    Prototype (d <> d', g `F.andGadgetry` g', t `F.andTrigger` t', f `F.andCreate` f')

orPrototype
    :: Prototype o1 ols d1 dtls p1 plns a1 acts c1 cmds
    -> Prototype o2 ols d2 dtls p2 plns a2 acts c2 cmds
    -> Prototype (Append o1 o2) ols (Append d1 d2) dtls (Append p1 p2) plns (AppendUnique a1 a2) acts (AppendUnique c1 c2) cmds
orPrototype (Prototype (d, g, t, f)) (Prototype (d', g', t', f')) =
         Prototype (d F.<<|>> d', g `F.orGadgetry` g', t `F.orTrigger` t', f `F.andCreate` f')

-- | identity for 'andPrototype' and 'orPrototype'
blank :: Prototype '[] ols '[] dtls '[] plns '[] acts '[] cmds
blank = Prototype (mempty, F.noop, F.ignore, F.idle)

statically :: F.Display dtls plns -> Prototype '[] ols '[] dtls '[] plns '[] acts '[] cmds
statically d = Prototype (d, F.noop, F.ignore, F.idle)

dynamically :: F.Gadgetry dtls plns a acts c cmds  -> Prototype '[] ols '[] dtls '[] plns a acts c cmds
dynamically g = Prototype (mempty, g, F.Trigger (Proxy, mempty), F.idle)

triggering :: F.Trigger a acts -> Prototype '[] ols '[] dtls '[] pln a acts '[] cmds
triggering t = Prototype (mempty, F.Gadgetry (Proxy, Proxy, empty), t, F.idle)

creating :: F.Create o ols d dtls p plns acts -> Prototype o ols d dtls p plns '[] acts '[] cmds
creating f = Prototype (mempty, F.noop, F.ignore, f)
