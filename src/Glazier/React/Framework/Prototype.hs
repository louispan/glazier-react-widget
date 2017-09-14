{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Framework.Prototype where

import Data.Diverse
import Data.Kind
import Data.Proxy
import Data.Semigroup (Semigroup(..))
import qualified Glazier.React.Framework.Builder as F
import qualified Glazier.React.Framework.Display as F
import qualified Glazier.React.Framework.Handler as F
import qualified Glazier.React.Framework.Trigger as F
import qualified Glazier.React.Framework.Widget as F

newtype Prototype (r :: [Type]) reqs
                  (s :: [Type]) specs
                  (a :: [Type]) (h :: [Type]) acts
                  (c :: [Type]) cmds =
    Prototype ( F.Display specs
              , F.Builder r reqs s specs a acts c cmds
              , F.Triggers a acts
              , F.Handler (F.Design specs) h acts c cmds)

-- instance Semigroup (Prototype '[] reqs '[] specs '[] '[] acts '[] cmds) where
--     _ <> _ = Prototype (mempty, mempty, mempty, mempty)

-- instance Monoid (Prototype '[] reqs '[] specs '[] '[] acts '[] cmds) where
--     mempty = Prototype (mempty, mempty, mempty, mempty)
--     mappend = (<>)

-- | identity for 'andPrototype'
dummy :: Prototype '[] reqs '[] specs '[] '[] acts '[] cmds
dummy = Prototype (mempty, F.idle, F.boring, F.ignore)

-- | The action and command types are merged, not appended
andPrototype
    :: Prototype r1 reqs s1 specs a1 h1 acts c1 cmds
    -> Prototype r2 reqs s2 specs a2 h2 acts c2 cmds
    -> Prototype (Append r1 r2) reqs
                 (Append s1 s2) specs
                 (AppendUnique a1 a2) (Append h1 h2) acts
                 (AppendUnique c1 c2) cmds
andPrototype (Prototype (d, b, t, h)) (Prototype (d', b', t', h')) =
    prototyping (d <> d')
                (b `F.andBuilder` b')
                (t `F.andTriggers` t')
                (h `F.orHandler` h')

prototyping
    :: F.Display specs
    -> F.Builder r reqs s specs ba acts bc cmds
    -> F.Triggers ta acts
    -> F.Handler (F.Design specs) h acts hc cmds
    -> Prototype r reqs s specs (AppendUnique ba ta) h acts (AppendUnique bc hc) cmds
prototyping disp (F.Builder (_, _, frmSpec, mkSpec, activateD)) (F.Triggers (_, ts)) (F.Handler (ha, _, hdl)) =
    Prototype
        ( disp
        , F.Builder (Proxy, Proxy, frmSpec, mkSpec, activateD)
        , F.Triggers (Proxy, ts)
        , F.Handler (ha, Proxy, hdl))

building :: F.Builder r reqs s specs a acts c cmds -> Prototype r reqs s specs a '[] acts c cmds
building b = prototyping mempty b F.boring F.ignore

displaying :: F.Display specs -> Prototype '[] reqs '[] specs '[] '[] acts '[] cmds
displaying d = prototyping d F.idle F.boring F.ignore

triggering :: F.Triggers a acts -> Prototype '[] reqs '[] specs a '[] acts '[] cmds
triggering t = prototyping mempty F.idle t F.ignore

handling :: F.Handler (F.Design specs) h acts c cmds -> Prototype '[] reqs '[] specs '[] h acts c cmds
handling h = prototyping mempty F.idle F.boring h
