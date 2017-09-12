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
import Data.Semigroup (Semigroup(..))
import qualified Glazier.React.Framework.Builder as F
import qualified Glazier.React.Framework.Display as F
import qualified Glazier.React.Framework.Handler as F
import qualified Glazier.React.Framework.Trigger as F
import qualified Glazier.React.Framework.Widget as F

newtype Prototype (r :: [Type]) reqs
                  (s :: [Type]) specs
                  (ba :: [Type]) (ta :: [Type]) (ha :: [Type]) acts
                  (bc :: [Type]) (hc :: [Type]) cmds =
    Prototype ( F.Builder r reqs s specs ba acts bc cmds
              , F.Display specs
              , F.Triggers ta acts
              , F.Handler (F.Design specs) ha acts hc cmds)

instance Semigroup (Prototype '[] reqs '[] specs '[] '[] '[] acts '[] '[] cmds) where
    _ <> _ = Prototype (mempty, mempty, mempty, mempty)

instance Monoid (Prototype '[] reqs '[] specs '[] '[] '[] acts '[] '[] cmds) where
    mempty = Prototype (mempty, mempty, mempty, mempty)
    mappend = (<>)

-- | mempty is also identity for 'andPrototype'
-- The action and command types are merged, not appended
andPrototype
    :: Prototype r1 reqs s1 specs ba1 ta1 ha1 acts bc1 hc1 cmds
    -> Prototype r2 reqs s2 specs ba2 ta2 ha2 acts bc2 hc2 cmds
    -> Prototype (Append r1 r2) reqs
                 (Append s1 s2) specs
                 (AppendUnique ba1 ba2) (AppendUnique ta1 ta2) (Append ha1 ha2) acts
                 (AppendUnique bc1 bc2) (AppendUnique hc1 hc2) cmds
andPrototype (Prototype (b, d, t, h)) (Prototype (b', d', t', h')) =
    Prototype ( b `F.andBuilder` b'
              , d <> d'
              , t `F.andTriggers` t'
              , h `F.orHandler` h')


building :: F.Builder r reqs s specs ba acts bc cmds -> Prototype r reqs s specs ba '[] '[] acts bc '[] cmds
building b = Prototype (b, mempty, mempty, mempty)

displaying :: F.Display specs -> Prototype '[] reqs '[] specs '[] '[] '[] acts '[] '[] cmds
displaying d = Prototype (mempty, d, mempty, mempty)

triggering :: F.Triggers ta acts -> Prototype '[] reqs '[] specs '[] ta '[] acts '[] '[] cmds
triggering t = Prototype (mempty, mempty, t, mempty)

handling :: F.Handler (F.Design specs) ha acts hc cmds -> Prototype '[] reqs '[] specs '[] '[] ha acts '[] hc cmds
handling h = Prototype (mempty, mempty, mempty, h)
