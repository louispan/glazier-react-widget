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
-- import qualified Glazier.React.Framework.Executor as F
-- import qualified Glazier.React.Framework.Firsts as F
-- import qualified Glazier.React.Framework.Gadgetry as F
-- import qualified Glazier.React.Framework.Trigger as F

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
blank :: Prototype '[] atrs '[] dtls '[] plns
blank = Prototype (F.idle, mempty, mempty)

displaying :: F.Display dtls plns -> Prototype '[] atrs '[] dtls '[] plns
displaying d = Prototype (F.idle, d, mempty)

-- triggering
--     :: Monad m
--     => F.Trigger t trigs a acts
--     -> Prototype m
--                  '[] atrs
--                  '[] dtls
--                  '[] pln
--                  t trigs
--                  a '[] acts
--                  '[] '[] cmds
--                  '[] envs
-- triggering t = Prototype (F.idle, mempty, t, F.noop, F.ignore)

-- building
--     :: Monad m
--     => F.Builder o atrs d dtls p plns acts
--     -> Prototype m
--                  o atrs
--                  d dtls
--                  p plns
--                  '[] trigs
--                  '[] '[] acts
--                  '[] '[] cmds
--                  '[] envs
-- building b = Prototype (b, mempty, F.boring, F.noop, F.ignore)

-- executing
--     :: F.Executor m c cmds e envs
--     -> Prototype m
--                  '[] atrs
--                  '[] dtls
--                  '[] plns
--                  '[] trigs
--                  '[] '[] acts
--                  '[] c cmds
--                  e envs
-- executing e = Prototype (F.idle, mempty, F.boring, F.noop, e)
