{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Framework.Core.Handler where

import Control.Applicative
import Data.Diverse.Profunctor
import Data.Semigroup
import qualified Glazier.React.Framework.Core.Gate as F
import qualified Glazier.React.Framework.Core.Model as F
import qualified Glazier.React.Framework.Core.Obj as F
import qualified Glazier.React.Framework.Core.Topic as F

type Handler m r a b = F.Topic r (F.Gate (m ())) a b

type ObjHandler m v s a b = Handler m (F.Obj v s) a b
type ProtoHandler m v s a b = Handler m (F.Scene m v s) a b

toHandler :: (r -> (b -> x) -> a -> x) -> F.Topic r (F.Gate x) a b
toHandler f = F.Topic (F.Gate <$> f)

fromHandler :: F.Topic r (F.Gate x) a b -> r -> (b -> x) -> a -> x
fromHandler (F.Topic f) = F.runGate <$> f

nulHandler :: Applicative m => Handler m r (Which '[]) (Which '[])
nulHandler = F.Topic . const $ F.pinned (pure ())

orHandler :: ChooseBetween a1 a2 a3 b1 b2 b3
    => Handler m r (Which a1) (Which b1)
    -> Handler m r (Which a2) (Which b2)
    -> Handler m r (Which a3) (Which b3)
orHandler (F.Topic f) (F.Topic g) = F.Topic $ \r -> (f r) `F.orGate` (g r)
infixr 6 `orHandler` -- like mappend

andHandler :: (Applicative m, ChooseBoth b1 b2 b3)
    => Handler m r a (Which b1)
    -> Handler m r a (Which b2)
    -> Handler m r a (Which b3)
andHandler (F.Topic f) (F.Topic g) = F.Topic $ \r -> F.andGate (liftA2 (<>)) (f r) (g r)
infixr 6 `andHandler` -- like mappend



-- wack :: s ->                     m () -- activator (and with *>)
-- wack :: s ->                a -> m () -- handler (and with choosefrom with Op (contravariant) ?)
-- wack :: s -> (b -> m ()) -> () -> m () -- ex activator
-- wack :: s -> (b -> m ()) ->  a -> m () -- ex handler
