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
import Data.Semigroup
import qualified Glazier.React.Framework.Builder as F
import qualified Glazier.React.Framework.Display as F
import qualified Glazier.React.Framework.Trigger as F

newtype Prototype v (r :: [Type]) reqs (s :: [Type]) specs (t :: [Type]) acts =
    Prototype ( F.Builder v r reqs s specs
              , F.Display specs
              , F.Trigger t acts
              )

-- | The action and command types are merged, not appended
andPrototype
    :: Prototype v r1 reqs s1 specs t1 acts
    -> Prototype v r2 reqs s2 specs t2 acts
    -> Prototype v (Append r1 r2) reqs (Append s1 s2) specs (AppendUnique t1 t2) acts
andPrototype (Prototype (b, d, t)) (Prototype (b', d', t')) =
    Prototype ( b `F.andBuilder` b'
              , d <> d'
              , t `F.andTrigger` t')

-- | identity for 'andPrototype'
dummy :: Prototype v '[] reqs '[] specs '[] acts
dummy = Prototype (F.idle, mempty, F.boring)

building :: F.Builder v r reqs s specs -> Prototype v r reqs s specs '[] acts
building b = Prototype (b, mempty, F.boring)

displaying :: F.Display specs -> Prototype v '[] reqs '[] specs '[] acts
displaying d = Prototype (F.idle, d, F.boring)

triggering :: F.Trigger t acts -> Prototype v '[] reqs '[] specs t acts
triggering t = Prototype (F.idle, mempty, t)
