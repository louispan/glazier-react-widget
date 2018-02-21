{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Framework.Core.Handler where

import Control.Arrow
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Cont.Extras as TE
import Data.Diverse.Profunctor
import qualified Glazier.React.Framework.Core.Model as F
import qualified Glazier.React.Framework.Core.Obj as F

-- | Handle a input @a@ and fire a event @b@
type Handler m s a b = s -> a -> ContT () m b
type ObjHandler m v s a b = Handler m (F.Obj v s) a b
type SceneHandler m v s a b = Handler m (F.Scene m v s) a b

-- | Identify for 'orHandler'' or 'andHandler''
nulHandler' :: Applicative m => Handler m r (Which '[]) ()
nulHandler' _ _ = ContT $ \_ -> pure ()

-- Run left after the right.
-- A binary associative function for 'nulHandler''.
andHandler' :: (Applicative m)
    => Handler m s a ()
    -> Handler m s a ()
    -> Handler m s a ()
andHandler' f g s a = (f s a) `TE.seqContT` (g s a)
infixr 6 `andHandler'` -- like mappend

-- | Run one or the other.
-- Compile error if types in @a1@ are not distinct from types in @a2@
-- A binary associative function for 'nulHandler''.
orHandler' :: forall m s a1 a2 a3. ChooseFrom a1 a2 a3
    => Handler m s (Which a1) ()
    -> Handler m s (Which a2) ()
    -> Handler m s (Which a3) ()
orHandler' f g s a = case reinterpret @a2 @a3 a of
    Left a1 -> f s a1
    Right a2 -> g s a2
infixr 6 `orHandler'` -- like mappend

-- | Identify for 'orHandler' or 'andHandler'
nulHandler :: Applicative m => Handler m r (Which '[]) (Which '[])
nulHandler _ _ = ContT $ \_ -> pure ()

-- | Run one or the other.
-- Compile error if types in @a1@ are not distinct from types in @a2@
-- A binary associative function for 'nulHandler'.
orHandler :: ChooseBetween a1 a2 a3 b1 b2 b3
    => Handler m s (Which a1) (Which b1)
    -> Handler m s (Which a2) (Which b2)
    -> Handler m s (Which a3) (Which b3)
orHandler f g s = runKleisli $ Kleisli (f s) +||+ Kleisli (g s)
infixr 6 `orHandler` -- like mappend

-- Run left after the right.
-- A binary associative function for 'nulHandler'.
andHandler :: (Applicative m, ChooseBoth b1 b2 b3)
    => Handler m s a (Which b1)
    -> Handler m s a (Which b2)
    -> Handler m s a (Which b3)
andHandler f g s a = (diversify <$> f s a) `TE.seqContT` (diversify <$> g s a)
infixr 6 `andHandler` -- like mappend

thenHandler :: Applicative m
    => Handler m s a b
    -> Handler m s b c
    -> Handler m s a c
thenHandler f g s a = f s a >>= g s

thenHandler' :: (Pretend a2 b1 b2 b3, Applicative m)
    => Handler m s a (Which b1)
    -> Handler m s (Which a2) (Which b2)
    -> Handler m s a (Which b3)
thenHandler' f g = thenHandler f (pretend g)

maybeHandle :: Applicative m
    => Handler m s a b
    -> Handler m s (Maybe a) b
maybeHandle hdl s ma = case ma of
    Nothing -> ContT $ \_ -> pure ()
    Just a -> hdl s a

-- | A friendlier constraint synonym for 'pretend'.
type Pretend a2 a3 b2 b3 =
    ( Reinterpret a2 a3
    , b3 ~ Append (Complement a3 a2) b2 -- ^ redundant constraint: but limits the output type
    , Diversify b2 b3
    , Diversify (Complement a3 a2) b3
    )

-- | Change the types the handler can handle by passing any extra
-- input directly to the output.
-- @a2@ contains the input type to convert into
-- AllowAmbiguousTypes@: Use @TypeApplications@ instead of @Proxy@ to specify @a3@
pretend :: forall a3 m s a2 b2 b3.
    (Pretend a2 a3 b2 b3)
    => Handler m s (Which a2) (Which b2)
    -> Handler m s (Which a3) (Which b3)
pretend hdl = go <$> hdl
  where
    go k a = case reinterpret a of
        Left a' -> pure $ diversify a'
        Right a' -> diversify <$> k a'
