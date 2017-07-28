{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Framework.Prototype
    ( Prototype(..)
    , dummy
    , statically
    , dynamically
    , triggering
    -- , attributes
    -- , componentize
    ) where

import Data.Diverse
import Data.Kind
import Data.Proxy
import Data.Semigroup
import qualified Glazier.React.Framework.Attach as F
import qualified Glazier.React.Framework.Display as F
import qualified Glazier.React.Framework.Gizmo as F
import qualified Glazier.React.Framework.Trigger as F

newtype Prototype (o :: [Type]) ols (d :: [Type]) dtls (p :: [Type]) plns (a :: [Type]) acts (c :: [Type]) cmds =
    Prototype (Proxy c, F.Display dtls plns, F.Trigger a acts, F.Gizmo o ols d dtls p plns acts cmds)

-- | The action and command types are merged, not appended
instance (o3 ~ Append o1 o2, d3 ~ Append d1 d2, p3 ~ Append p1 p2, a3 ~ AppendUnique a1 a2, c3 ~ AppendUnique c1 c2) =>
         F.Attach (Prototype o1 ols d1 dtls p1 plns a1 acts c1 cmds)
                (Prototype o2 ols d2 dtls p2 plns a2 acts c2 cmds)
                (Prototype o3 ols d3 dtls p3 plns a3 acts c3  cmds) where
     Prototype (_, d, t, g) +<>+ Prototype (_, d', t', g') =
         Prototype (Proxy, d <> d', t F.+<>+ t', g F.+<>+ g')
     Prototype (_, d, t, g) +<|>+ Prototype (_, d', t', g') =
         Prototype (Proxy, d <> d', t F.+<|>+ t', g F.+<|>+ g')

instance F.AttachId (Prototype '[] ols '[] dtls '[] plns '[] acts '[] cmds) where
    aempty = dummy

-- | identity for 'F.Attach'
dummy :: Prototype '[] ols '[] dtls '[] plns '[] acts '[] cmds
dummy = Prototype (Proxy, F.blank, F.ignore, F.noop)

statically :: F.Display dtls plns -> Prototype '[] ols '[] dtls '[] plns '[] acts '[] cmds
statically d = Prototype (Proxy, d, F.ignore, F.noop)

dynamically :: F.Gizmo o ols d dtls p plns acts cmds  -> Prototype o ols d dtls p plns a acts c cmds
dynamically g = Prototype (Proxy, mempty, F.Trigger (Proxy, mempty), g)

triggering :: F.Trigger a acts -> Prototype '[] ols '[] dtls '[] pln a acts '[] cmds
triggering t = Prototype (Proxy, mempty, t, F.noop)
