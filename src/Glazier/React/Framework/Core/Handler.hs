{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Framework.Core.Handler where

import Control.Arrow
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Cont.Extras as E
import Data.Diverse.Profunctor
import qualified Glazier.React.Framework.Core.Model as F
import qualified Glazier.React.Framework.Core.Obj as F

type Handler' m s a = s -> a -> m ()
type Handler m s a b = s -> a -> ContT () m b

type ObjHandler' m v s a = Handler' m (F.Obj v s) a
type ObjHandler m v s a b = Handler m (F.Obj v s) a b

type SceneHandler' m v s a = Handler' m (F.Scene m v s) a
type SceneHandler m v s a b = Handler m (F.Scene m v s) a b

-- | Convert the original ContT to a ContT that
-- doens't call it's continuation, by 'const'ing the original contination
-- to 'pure'.
terminate :: forall b m . Applicative m => ContT () m () -> ContT () m b
terminate = withContT (const $ pure)

-- A variation of 'terminate' which  also fixes the result a @(Which '[])@,
-- so this ContT can be run with  'Data.Diverse.Which.impossible'.
-- @ContT r m (Which '[])@ is equivanlent to @m r@
terminate' :: Applicative m => ContT () m () -> ContT () m (Which '[])
terminate' = terminate @(Which '[])

nulHandler :: Applicative m => Handler m r (Which '[]) (Which '[])
nulHandler _ _ = ContT $ \_ -> pure ()

-- | run one or the other.
-- Compile error if types in @a1@ are not distinct from types in @a2@
orHandler' :: forall m s a1 a2 a3. ChooseFrom a1 a2 a3
    => Handler' m s (Which a1)
    -> Handler' m s (Which a2)
    -> Handler' m s (Which a3)
orHandler' f g s a = case reinterpret @a2 @a3 a of
    Left a1 -> f s a1
    Right a2 -> g s a2
infixr 6 `orHandler'` -- like mappend

-- | run one or the other.
-- Compile error if types in @a1@ are not distinct from types in @a2@
orHandler :: ChooseBetween a1 a2 a3 b1 b2 b3
    => Handler m s (Which a1) (Which b1)
    -> Handler m s (Which a2) (Which b2)
    -> Handler m s (Which a3) (Which b3)
orHandler f g s = runKleisli $ Kleisli (f s) +||+ Kleisli (g s)
infixr 6 `orHandler` -- like mappend

-- run left after the right.
andHandler :: (Applicative m, ChooseBoth b1 b2 b3)
    => Handler m s a (Which b1)
    -> Handler m s a (Which b2)
    -> Handler m s a (Which b3)
andHandler f g s a = (diversify <$> f s a) `E.seqContT` (diversify <$> g s a)
infixr 6 `andHandler` -- like mappend

-- wack :: s ->                     m () -- activator (and with *>)
-- wack :: s ->                a -> m () -- handler (and with choosefrom with Op (contravariant) ?)
-- wack :: s -> (b -> m ()) -> () -> m () -- ex activator
-- wack :: s -> (b -> m ()) ->  a -> m () -- ex handler

-- wack ::      s ->                m () -- activator (and with *>)
-- wack :: a -> s ->             -> m () -- handler (and with choosefrom with Op (contravariant) ?)
-- wack ::      s -> (b -> m ()) -> m () -- ex activator
-- wack :: a -> s -> (b -> m ()) -> m () -- ex handler

-- wock :: s ->      m () -- activator (and with *>)
-- wock :: s -> a -> m () -- handler (and with choosefrom with Op (contravariant) ?)
-- wock :: s ->      ContT () m b -- ex activator
-- wock :: s -> a -> ContT () m b -- ex handler
