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
import qualified Data.DList as D
import qualified Glazier.React.Framework.Builder as F
import qualified Glazier.React.Framework.Display as F
import qualified Glazier.React.Framework.Widget as F

newtype Prototype (r :: [Type]) reqs (s :: [Type]) specs =
    Prototype ( F.Builder r reqs s specs
              , F.Display specs
              , D.DList (F.Trigger specs))

-- | The action and command types are merged, not appended
andPrototype
    :: Prototype r1 reqs s1 specs
    -> Prototype r2 reqs s2 specs
    -> Prototype (Append r1 r2) reqs
                 (Append s1 s2) specs
andPrototype (Prototype (b, d, t)) (Prototype (b', d', t')) =
    Prototype ( b `F.andBuilder` b'
              , d <> d'
              , t <> t')

-- | identity for 'andPrototype'
dummy :: Prototype '[] reqs '[] specs
dummy = Prototype (F.idle, mempty, mempty)

displaying :: F.Display specs -> Prototype '[] reqs '[] specs
displaying d = Prototype (F.idle, d, mempty)

triggering :: D.DList (F.Trigger specs)
           -> Prototype '[] reqs '[] specs
triggering t = Prototype (F.idle, mempty, t)

building
    :: F.Builder r reqs s specs
    -> Prototype r reqs s specs
building b = Prototype (b, mempty, mempty)
