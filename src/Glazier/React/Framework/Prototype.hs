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

newtype Prototype (a :: [Type]) atrs (d :: [Type]) dtls (p :: [Type]) plns =
    Prototype (F.Builder a atrs d dtls p plns, F.Display dtls plns, D.DList (F.Trigger dtls plns))

-- | The action and command types are merged, not appended
andPrototype
    :: Prototype a1 atrs d1 dtls p1 plns
    -> Prototype a2 atrs d2 dtls p2 plns
    -> Prototype (Append a1 a2) atrs
                 (Append d1 d2) dtls
                 (Append p1 p2) plns
andPrototype (Prototype (b, d, t)) (Prototype (b', d', t')) =
    Prototype ( b `F.andBuilder` b'
              , d <> d'
              , t <> t')

-- | identity for 'andPrototype'
dummy :: Prototype '[] atrs '[] dtls '[] plns
dummy = Prototype (F.idle, mempty, mempty)

displaying :: F.Display dtls plns -> Prototype '[] atrs '[] dtls '[] plns
displaying d = Prototype (F.idle, d, mempty)

triggering :: D.DList (F.Trigger dtls plns)
           -> Prototype '[] atrs '[] dtls '[] plns
triggering t = Prototype (F.idle, mempty, t)

building
    :: F.Builder a atrs d dtls p plns
    -> Prototype a atrs
                 d dtls
                 p plns
building b = Prototype (b, mempty, mempty)
