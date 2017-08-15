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

newtype Prototype (a :: [Type]) atrs (d :: [Type]) dtls =
    Prototype (F.Builder a atrs d dtls, F.Display dtls, D.DList (F.Trigger dtls))

-- | The action and command types are merged, not appended
andPrototype
    :: Prototype a1 atrs d1 dtls
    -> Prototype a2 atrs d2 dtls
    -> Prototype (Append a1 a2) atrs
                 (Append d1 d2) dtls
andPrototype (Prototype (b, d, t)) (Prototype (b', d', t')) =
    Prototype ( b `F.andBuilder` b'
              , d <> d'
              , t <> t')

-- | identity for 'andPrototype'
dummy :: Prototype '[] atrs '[] dtls
dummy = Prototype (F.idle, mempty, mempty)

displaying :: F.Display dtls -> Prototype '[] atrs '[] dtls
displaying d = Prototype (F.idle, d, mempty)

triggering :: D.DList (F.Trigger dtls)
           -> Prototype '[] atrs '[] dtls
triggering t = Prototype (F.idle, mempty, t)

building
    :: F.Builder a atrs d dtls
    -> Prototype a atrs
                 d dtls
building b = Prototype (b, mempty, mempty)
