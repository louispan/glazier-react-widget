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
import qualified Glazier.React.Framework.Executor as F
import qualified Glazier.React.Framework.Handler as F
import qualified Glazier.React.Framework.Trigger as F
import qualified Glazier.React.Framework.Widget as F

newtype Prototype v (r :: [Type]) reqs
                    (s :: [Type]) specs
                    (t :: [Type]) (h :: [Type]) acts
                    (c :: [Type]) (e :: [Type]) cmds =
    Prototype ( F.Builder v r reqs s specs
              , F.Display specs
              , F.Triggers t acts
              , F.Handler v (F.Design specs) h acts c cmds
              , F.Executor e cmds)

instance Semigroup (Prototype v '[] reqs '[] specs '[] '[] acts '[] '[] cmds) where
    _ <> _ = Prototype (mempty, mempty, mempty, mempty, mempty)

instance Monoid (Prototype v '[] reqs '[] specs '[] '[] acts '[] '[] cmds) where
    mempty = Prototype (mempty, mempty, mempty, mempty, mempty)
    mappend = (<>)

-- | mempty is also identity for 'andPrototype'
-- The action and command types are merged, not appended
andPrototype
    :: Prototype v r1 reqs s1 specs t1 h1 acts c1 e1 cmds
    -> Prototype v r2 reqs s2 specs t2 h2 acts c2 e2 cmds
    -> Prototype v (Append r1 r2) reqs
                   (Append s1 s2) specs
                   (AppendUnique t1 t2) (Append h1 h2) acts
                   (AppendUnique c1 c2) (Append e1 e2) cmds
andPrototype (Prototype (b, d, t, h, e)) (Prototype (b', d', t', h', e')) =
    Prototype ( b `F.andBuilder` b'
              , d <> d'
              , t `F.andTriggers` t'
              , h `F.orHandler` h'
              , e `F.orExecutor` e')


-- building :: F.Builder v r reqs s specs -> Prototype v r reqs s specs '[] '[] acts '[] '[] cmds
-- building b = Prototype (b, mempty, mempty, mempty, mempty)

-- displaying :: F.Display specs -> Prototype v '[] reqs '[] specs '[] '[] acts '[] '[] cmds
-- displaying d = Prototype (mempty, d, mempty, mempty, mempty)

-- triggering :: F.Triggers t acts -> Prototype v '[] reqs '[] specs t '[] acts '[] '[] cmds
-- triggering t = Prototype (mempty, mempty, t, mempty, mempty)

-- handling :: F.Handler v (F.Design specs) h acts c cmds -> Prototype v '[] reqs '[] specs '[] h acts c '[] cmds
-- handling h = Prototype (mempty, mempty, mempty, h, mempty)
