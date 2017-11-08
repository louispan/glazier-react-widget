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
import Data.Diverse.Profunctor
import qualified Data.DList as DL
import Data.Foldable
import Data.IORef
import Data.Kind
import Data.Profunctor
-- import qualified Glazier.React as R
import Glazier.React.Framework.Core as F
import qualified Parameterized.Data.Monoid as P
import qualified Parameterized.TypeLevel as P

newtype Handler (m :: Type -> Type) r a b = Handler
    { runHandler :: r -> a -> m (DL.DList b)
    }

-- | Uses ReifiedLens' to avoid impredicative polymorphism
type Handler' m v s a b = Handler m (IORef v, ReifiedLens' v s) a b

instance Functor m => Functor (Handler m r a) where
    fmap f (Handler hdl) = Handler $ \env a -> fmap f <$> hdl env a

instance Functor m => Profunctor (Handler m r) where
    dimap f g (Handler hdl) = Handler $ \env a -> fmap g <$> hdl env (f a)

instance Functor m => Strong (Handler m r) where
    first' (Handler hdl) = Handler $ \env (a, c) -> fmap (\b -> (b, c)) <$> hdl env a
    second' (Handler hdl) = Handler $ \env (c, a) -> fmap (\b -> (c, b)) <$> hdl env a

instance Applicative m => Choice (Handler m r) where
    left' (Handler hdl) = Handler $ \env e -> case e of
        Right c -> pure $ DL.singleton $ Right c
        Left a -> fmap Left <$> hdl env a
    right' (Handler hdl) = Handler $ \env e -> case e of
        Left c -> pure $ DL.singleton $ Left c
        Right a -> fmap Right <$> hdl env a

instance Monad m => C.Category (Handler m r) where
    id = Handler $ \_ -> pure . DL.singleton
    (Handler hdl) . (Handler hdl') = Handler $ \env a -> do
        bs <- hdl' env a
        fold <$> traverse (hdl env) (DL.toList bs)

instance Monad m => Arrow (Handler m r) where
    arr f = rmap f C.id
    first = first'
    second = second'

instance Monad m => ArrowChoice (Handler m r) where
    left = left'
    right = right'

-- instance R.MonadReactor m => F.IORefModel
--         (Handler m s a b)
--         (Handler m (IORef v, ReifiedLens' v s) a b) where
--     ioRefModel (Handler hdl) = Handler $ \(ref, Lens this) a -> do
--         obj <- R.doReadIORef ref
--         hdl (obj ^. this) a

-- | Ignore certain inputs
suppressHandler :: Applicative m => (a' -> Maybe a) -> Handler m r a b -> Handler m r a' b
suppressHandler f (Handler hdl) = Handler $ \env a' -> case f a' of
    Nothing -> pure DL.empty
    Just a -> hdl env a

-- | Ignore certain outputs
filterHandler :: Applicative m => (b -> Maybe b') -> Handler m r a b -> Handler m r a b'
filterHandler f (Handler hdl) = Handler $ \env a -> foldMap go <$> hdl env a
  where
    go b = case f b of
        Nothing -> DL.empty
        Just b' -> DL.singleton b'

-----------------------------------------------

newtype HandlerModeller m a b v s = HandlerModeller {
    runHandlerModeller :: Handler' m v s a b
    }

type instance F.Modeller (HandlerModeller m a b v) s = Handler' m v s a b

instance F.ViaModel (HandlerModeller m a b v) where
    viaModel l (Handler hdl) =
        Handler $ \(ref, Lens this) a -> hdl (ref, Lens (this.l)) a

-------------------------------------

newtype PHandler m r ab = PHandler
    { runPHandler :: Handler m r (P.At0 ab) (P.At1 ab)
    }

type instance P.PNullary (PHandler m r) (a, b) = Handler m r a b

-- | NB. This is also identity for 'Data.Diverse.Profunctor.+||+'
instance Applicative m => P.PMEmpty (PHandler m r) (Which '[], Which '[]) where
    pmempty = Handler $ \_ _ -> pure DL.empty

-- | Undecidableinstances!
instance ( Monad m
         , a3 ~ Append a1 a2
         , b3 ~ AppendUnique b1 b2
         , Reinterpret a2 a3
         , a1 ~ Complement a3 a2
         , Diversify b1 b3
         , Diversify b2 b3
         ) =>
         P.PSemigroup (PHandler m r) (Which a1, Which b1) (Which a2, Which b2) (Which a3, Which b3) where
    x `pmappend` y = x +||+ y

------------------------------------------------------

-- -- | Like unix @cat@, forward input to output.
-- idHandler :: Monad m => Handler m v s a a
-- idHandler = C.id
