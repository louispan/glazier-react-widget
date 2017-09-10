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

newtype Prototype v
                  (r :: [Type]) reqs
                  (s :: [Type]) specs
                  (ba :: [Type]) acts
                  (t :: [Type]) (h :: [Type]) acts'
                  (bc :: [Type]) (tc :: [Type]) cmds =
    Prototype ( F.Builder v r reqs s specs ba acts bc cmds
              , F.Display specs
              , F.Triggers t acts'
              , F.Handler v (F.Design specs) h acts' tc cmds)

instance Semigroup (Prototype v '[] reqs '[] specs '[] acts '[] '[] acts' '[] '[] cmds) where
    _ <> _ = Prototype (mempty, mempty, mempty, mempty)

instance Monoid (Prototype v '[] reqs '[] specs '[] acts '[] '[] acts' '[] '[] cmds) where
    mempty = Prototype (mempty, mempty, mempty, mempty)
    mappend = (<>)

-- | mempty is also identity for 'andPrototype'
-- The action and command types are merged, not appended
andPrototype
    :: Prototype v r1 reqs s1 specs ba1 acts t1 h1 acts' bc1 tc1 cmds
    -> Prototype v r2 reqs s2 specs ba2 acts t2 h2 acts' bc2 tc2 cmds
    -> Prototype v
                 (Append r1 r2) reqs
                 (Append s1 s2) specs
                 (AppendUnique ba1 ba2) acts
                 (AppendUnique t1 t2) (Append h1 h2) acts'
                 (AppendUnique bc1 bc2) (AppendUnique tc1 tc2) cmds
andPrototype (Prototype (b, d, t, h)) (Prototype (b', d', t', h')) =
    Prototype ( b `F.andBuilder` b'
              , d <> d'
              , t `F.andTriggers` t'
              , h `F.orHandler` h')


building :: F.Builder v r reqs s specs ba acts bc cmds -> Prototype v r reqs s specs ba acts '[] '[] acts' bc '[] cmds
building b = Prototype (b, mempty, mempty, mempty)

displaying :: F.Display specs -> Prototype v '[] reqs '[] specs '[] acts '[] '[] acts' '[] '[] cmds
displaying d = Prototype (mempty, d, mempty, mempty)

triggering :: F.Triggers t acts' -> Prototype v '[] reqs '[] specs '[] acts t '[] acts' '[] '[] cmds
triggering t = Prototype (mempty, mempty, t, mempty)

handling :: F.Handler v (F.Design specs) h acts' tc cmds -> Prototype v '[] reqs '[] specs '[] acts '[] h acts' '[] tc cmds
handling h = Prototype (mempty, mempty, mempty, h)
