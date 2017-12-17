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
import qualified Glazier.React.Framework.Core as F
import qualified Glazier.React.Framework.Executor as F
import qualified Parameterized.Data.Monoid as P
import qualified Parameterized.TypeLevel as P

newtype Handler (m :: Type -> Type) r x a b = Handler
    { runHandler :: F.Executor x () -> r -> a -> m (DL.DList b)
    }

instance Functor m => Functor (Handler m r x a) where
    fmap f (Handler hdl) = Handler $ \exec env a -> fmap f <$> hdl exec env a

instance Functor m => Profunctor (Handler m r x) where
    dimap f g (Handler hdl) = Handler $ \exec env a -> fmap g <$> hdl exec env (f a)

instance Functor m => Strong (Handler m r x) where
    first' (Handler hdl) = Handler $ \exec env (a, c) -> fmap (\b -> (b, c)) <$> hdl exec env a
    second' (Handler hdl) = Handler $ \exec env (c, a) -> fmap (\b -> (c, b)) <$> hdl exec env a

instance Applicative m => Choice (Handler m r x) where
    left' (Handler hdl) = Handler $ \exec env e -> case e of
        Right c -> pure $ DL.singleton $ Right c
        Left a -> fmap Left <$> hdl exec env a
    right' (Handler hdl) = Handler $ \exec env e -> case e of
        Left c -> pure $ DL.singleton $ Left c
        Right a -> fmap Right <$> hdl exec env a

instance Monad m => C.Category (Handler m r x) where
    id = Handler $ \_ _ -> pure . DL.singleton
    (Handler hdl) . (Handler hdl') = Handler $ \exec env a -> do
        bs <- hdl' exec env a
        fold <$> traverse (hdl exec env) (DL.toList bs)

instance Monad m => Arrow (Handler m r x) where
    arr f = rmap f C.id
    first = first'
    second = second'

instance Monad m => ArrowChoice (Handler m r x) where
    left = left'
    right = right'

-- instance R.MonadReactor m => F.IORefModel
--         (Handler m s a b)
--         (Handler m (IORef v, ReifiedLens' v s) a b) where
--     ioRefModel (Handler hdl) = Handler $ \(ref, Lens this) a -> do
--         obj <- R.doReadIORef ref
--         hdl (obj ^. this) a

-- | Ignore certain inputs contravariantly
suppressHandlerInput :: Applicative m => (a -> Maybe a') -> Handler m r x a' b -> Handler m r x a b
suppressHandlerInput f (Handler hdl) = Handler $ \exec env a -> case f a of
    Nothing -> pure DL.empty
    Just a' -> hdl exec env a'

-- | More descriptive name for 'lmap'
contramapHandlerInput :: Functor m => (a -> a') -> Handler m r x a' b -> Handler m r x a b
contramapHandlerInput = lmap

-- | Ignore certain outputs
filterHandlerOutput :: Applicative m => (b -> Maybe b') -> Handler m r x a b -> Handler m r x a b'
filterHandlerOutput f (Handler hdl) = Handler $ \exec env a -> foldMap go <$> hdl exec env a
  where
    go b = case f b of
        Nothing -> DL.empty
        Just b' -> DL.singleton b'

-- | More descriptive name for 'rmap'
mapHandlerOutput :: Functor m => (b -> b') -> Handler m r x a b -> Handler m r x a b'
mapHandlerOutput = rmap


-- | Ignore certain commands contravariantly
suppressHandlerExecutor :: (x -> Maybe x') ->  Handler m r x a b -> Handler m r x' a b
suppressHandlerExecutor f (Handler hdl) = Handler $ \exec env a -> hdl (F.suppressExecutor f exec) env a

-- | Map a function to the commands contravariantly
contramapHandlerExecutor :: (x -> x') -> Handler m r x a b -> Handler m r x' a b
contramapHandlerExecutor f (Handler hdl) = Handler $ \exec env a -> hdl (F.contramapExecutor f exec) env a

-----------------------------------------------

-- | Uses ReifiedLens' to avoid impredicative polymorphism
type RefHandler m v s x a b = Handler m (IORef v, ReifiedLens' v s) x a b

newtype RefHandlerModeller m x a b v s = RefHandlerModeller {
    runHandlerModeller :: RefHandler m v s x a b
    }

type instance F.Modeller (RefHandlerModeller m x a b v) s = RefHandler m v s x a b

instance F.ViaModel (RefHandlerModeller m x a b v) where
    viaModel l (Handler hdl) =
        Handler $ \exec (ref, Lens this) a -> hdl exec (ref, Lens (this.l)) a

-- toRefHandler :: R.MonadReactor m => Handler m s a b -> RefHandler m v s x a b
-- toRefHandler (Handler hdl) = Handler $ \(_, ref, Lens this) a -> do
--     obj <- R.doReadIORef ref
--     hdl (obj ^. this) a

toFacetedHandler :: Applicative m => Handler m r x a b -> Handler m r x (Which '[a]) (Which '[b])
toFacetedHandler hdl = suppressHandlerInput trial' (pickOnly <$> hdl)

-------------------------------------

newtype PHandler m r xab = PHandler
    { runPHandler :: Handler m r (P.At0 xab) (P.At1 xab) (P.At2 xab)
    }

type instance P.PNullary (PHandler m r) (x, a, b) = Handler m r x a b

-- | NB. This is also identity for 'Data.Diverse.Profunctor.+||+'
instance Applicative m => P.PMEmpty (PHandler m r) (Which '[], Which '[], Which '[]) where
    pmempty = Handler $ \_ _ _ -> pure DL.empty

-- | Undecidableinstances!
instance ( Monad m
         , x3 ~ AppendUnique x1 x2
         , Diversify x1 x3
         , Diversify x2 x3
         , a3 ~ Append a1 a2
         , b3 ~ AppendUnique b1 b2
         , Reinterpret a2 a3
         , a1 ~ Complement a3 a2
         , Diversify b1 b3
         , Diversify b2 b3
         ) =>
         P.PSemigroup (PHandler m r)
              (Which x1, Which a1, Which b1)
              (Which x2, Which a2, Which b2)
              (Which x3, Which a3, Which b3) where
    x `pmappend` y = (contramapHandlerExecutor diversify x)
        +||+ (contramapHandlerExecutor diversify y)

------------------------------------------------------

-- -- | Like unix @cat@, forward input to output.
-- idHandler :: Monad m => Handler m v s a a
-- idHandler = C.id
