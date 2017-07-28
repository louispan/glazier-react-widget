{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Framework.Prototype
    ( Prototype(..)
    , andPrototype
    , orPrototype
    , dummy
    , statically
    , dynamically
    , triggering
    ) where

import Control.Applicative
import Data.Diverse
import Data.Kind
import Data.Proxy
import Data.Semigroup
import qualified Glazier.React.Framework.Display as F
import qualified Glazier.React.Framework.Gizmo as F
import qualified Glazier.React.Framework.Trigger as F

newtype Prototype (o :: [Type]) ols (d :: [Type]) dtls (p :: [Type]) plns (a :: [Type]) acts (c :: [Type]) cmds =
    Prototype (F.Display dtls plns, F.Trigger a acts, F.Gizmo o ols d dtls p plns a acts c cmds)

-- | The action and command types are merged, not appended
andPrototype
    :: Prototype o1 ols d1 dtls p1 plns a1 acts c1 cmds
    -> Prototype o2 ols d2 dtls p2 plns a2 acts c2 cmds
    -> Prototype (Append o1 o2) ols (Append d1 d2) dtls (Append p1 p2) plns (AppendUnique a1 a2) acts (AppendUnique c1 c2) cmds
andPrototype (Prototype (d, t, g)) (Prototype (d', t', g')) =
         Prototype (d <> d', t `F.andTrigger` t', g `F.andGizmo` g')

orPrototype
    :: Prototype o1 ols d1 dtls p1 plns a1 acts c1 cmds
    -> Prototype o2 ols d2 dtls p2 plns a2 acts c2 cmds
    -> Prototype (Append o1 o2) ols (Append d1 d2) dtls (Append p1 p2) plns (AppendUnique a1 a2) acts (AppendUnique c1 c2) cmds
orPrototype (Prototype (d, t, g)) (Prototype (d', t', g')) =
         Prototype (d <> d', t `F.orTrigger` t', g `F.orGizmo` g')

-- | identity for 'andPrototype' and 'orPrototype'
dummy :: Prototype '[] ols '[] dtls '[] plns '[] acts '[] cmds
dummy = Prototype (F.blank, F.ignore, F.noop)

statically :: F.Display dtls plns -> Prototype '[] ols '[] dtls '[] plns '[] acts '[] cmds
statically d = Prototype (d, F.ignore, F.noop)

dynamically :: F.Gizmo o ols d dtls p plns a acts c cmds  -> Prototype o ols d dtls p plns a acts c cmds
dynamically g = Prototype (mempty, F.Trigger (Proxy, mempty), g)

triggering :: F.Trigger a acts -> Prototype '[] ols '[] dtls '[] pln a acts '[] cmds
triggering t = Prototype (mempty, t, F.gizmo (Proxy, Proxy, empty))
