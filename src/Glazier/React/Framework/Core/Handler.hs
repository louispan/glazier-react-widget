{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Framework.Core.Handler where

import Data.Diverse.Profunctor
import qualified Glazier.React.Framework.Core.Gate as F
import qualified Glazier.React.Framework.Core.Model as F
import qualified Glazier.React.Framework.Core.Obj as F
import qualified Glazier.React.Framework.Core.Topic as F

type Handler m r a b = F.Topic r (F.Gate (m ())) a b

type ObjHandler m v s a b = Handler m (F.Obj v s) a b
type ProtoHandler m v s a b = Handler m (F.Scene m v s) a b

nulHandler :: Applicative m => Handler m r (Which '[]) (Which '[])
nulHandler = F.Topic . const $ F.pinned (pure ())

plusHandler :: ChooseBetween a1 a2 a3 b1 b2 b3
    => Handler m r (Which a1) (Which b1)
    -> Handler m r (Which a2) (Which b2)
    -> Handler m r (Which a3) (Which b3)
plusHandler (F.Topic f) (F.Topic g) = F.Topic $ \r -> (f r) `F.plusGate` (g r)
infixr 6 `plusHandler` -- like mappend
