{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Framework.Handler where

import Control.Arrow
import qualified Control.Category as C
import Control.Lens
import Control.Parameterized as P
import Data.Diverse.Profunctor
import qualified Data.DList as DL
import Data.Foldable
import Data.IORef
import Data.Kind
import Data.Profunctor
import qualified Glazier.React as R
import Glazier.React.Framework.Core as F

-- | Uses ReifiedLens' to avoid impredicative polymorphism
newtype Handler (m :: Type -> Type) v s a b = Handler
    { runHandler :: IORef v -> ReifiedLens' v s -> a -> m (DL.DList b)
    }

instance Functor m => Functor (Handler m v s a) where
    fmap f (Handler hdl) = Handler $ \ref this a -> fmap f <$> hdl ref this a

instance Functor m => Profunctor (Handler m v s) where
    dimap f g (Handler hdl) = Handler $ \ref this a -> fmap g <$> hdl ref this (f a)

instance Functor m => Strong (Handler m v s) where
    first' (Handler hdl) = Handler $ \ref this (a, c) -> fmap (\b -> (b, c)) <$> hdl ref this a
    second' (Handler hdl) = Handler $ \ref this (c, a) -> fmap (\b -> (c, b)) <$> hdl ref this a

instance Applicative m => Choice (Handler m v s) where
    left' (Handler hdl) = Handler $ \ref this e -> case e of
        Right c -> pure $ DL.singleton $ Right c
        Left a -> fmap Left <$> hdl ref this a
    right' (Handler hdl) = Handler $ \ref this e -> case e of
        Left c -> pure $ DL.singleton $ Left c
        Right a -> fmap Right <$> hdl ref this a

instance Monad m => C.Category (Handler m v s) where
    id = Handler $ \_ _ -> pure . DL.singleton
    (Handler hdl) . (Handler hdl') = Handler $ \ref this a -> do
        bs <- hdl' ref this a
        fold <$> traverse (hdl ref this) (DL.toList bs)

instance Monad m => Arrow (Handler m v s) where
    arr f = rmap f C.id
    first = first'
    second = second'

instance Monad m => ArrowChoice (Handler m v s) where
    left = left'
    right = right'

newtype HandlerModeller m a b v s = HandlerModeller { runHandlerModeller :: Handler m v s a b }

instance F.IsModeller (Handler m v s a b) (HandlerModeller m a b v) s where
    toModeller = HandlerModeller
    fromModeller = runHandlerModeller

instance Monad m => F.ViaModel (HandlerModeller m a b v) where
    viaModel l (HandlerModeller (Handler hdl)) =
        HandlerModeller . Handler $ \ref (Lens this) a -> hdl ref (Lens (this.l)) a

instance R.MonadReactor m => F.IORefModel (Handler m s s a b) (Handler m v (IORef s) a b) where
    ioRefModel (Handler hdl) = Handler $ \ref (Lens this) a -> do
        obj <- R.doReadIORef ref
        hdl (obj ^. this) (Lens id) a

-- | Ignore certain inputs
suppressHandler :: Applicative m => (a' -> Maybe a) -> Handler m v s a b -> Handler m v s a' b
suppressHandler f (Handler hdl) = Handler $ \ref this a' -> case f a' of
    Nothing -> pure DL.empty
    Just a -> hdl ref this a

-- | Ignore certain outputs
filterHandler :: Applicative m => (b -> Maybe b') -> Handler m v s a b -> Handler m v s a b'
filterHandler f (Handler hdl) = Handler $ \ref this a -> foldMap go <$> hdl ref this a
  where
    go b = case f b of
        Nothing -> DL.empty
        Just b' -> DL.singleton b'

-------------------------------------

newtype HandlerPNullary m v s ab = HandlerPNullary
    { runHandlerPNullary :: Handler m v s (P.Fst ab) (P.Snd ab)
    }

instance IsPNullary (Handler m v s a b) (HandlerPNullary m v s) '(a, b) where
    toPNullary = HandlerPNullary
    fromPNullary = runHandlerPNullary

-- | NB. This is also identity for 'Data.Diverse.Profunctor.+||+'
instance Applicative m => P.PEmpty (HandlerPNullary m v s) '(Which '[], Which '[]) where
    pempty' = HandlerPNullary . Handler $ \_ _ _ -> pure DL.empty

-- | Undecidableinstances!
instance ( Monad m
         , a3 ~ Append a1 a2
         , b3 ~ AppendUnique b1 b2
         , Reinterpret a2 a3
         , a1 ~ Complement a3 a2
         , Diversify b1 b3
         , Diversify b2 b3
         ) =>
         P.PSemigroup (HandlerPNullary m v s) '(Which a1, Which b1) '(Which a2, Which b2) '(Which a3, Which b3) where
    (HandlerPNullary x) `pappend'` (HandlerPNullary y) = HandlerPNullary (x +||+ y)

-- -- | Like unix @cat@, forward input to output.
-- idHandler :: Monad m => Handler m v s a a
-- idHandler = C.id
