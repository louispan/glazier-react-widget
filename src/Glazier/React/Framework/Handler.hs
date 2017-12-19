{-# LANGUAGE AllowAmbiguousTypes #-}
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
import qualified Glazier.React.Framework.Core as F
import qualified Parameterized.Data.Monoid as P
import qualified Parameterized.TypeLevel as P

newtype Handler (m :: Type -> Type) r a b = Handler
    { runHandler :: r -> a -> m (DL.DList b)
    }

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
    id = Handler $ \_  -> pure . DL.singleton
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

-- | Ignore certain inputs contravariantly
suppressHandlerInput :: Applicative m => (a -> Maybe a') -> Handler m r a' b -> Handler m r a b
suppressHandlerInput f (Handler hdl) = Handler $ \env a -> case f a of
    Nothing -> pure DL.empty
    Just a' -> hdl env a'

-- | More descriptive name for 'lmap'
contramapHandlerInput :: Functor m => (a -> a') -> Handler m r a' b -> Handler m r a b
contramapHandlerInput = lmap

-- | Ignore certain outputs
filterHandlerOutput :: Applicative m => (b -> Maybe b') -> Handler m r a b -> Handler m r a b'
filterHandlerOutput f (Handler hdl) = Handler $ \env a -> foldMap go <$> hdl env a
  where
    go b = case f b of
        Nothing -> DL.empty
        Just b' -> DL.singleton b'

-- | More descriptive name for 'rmap' for Handler
mapHandlerOutput :: Functor m => (b -> b') -> Handler m r a b -> Handler m r a b'
mapHandlerOutput = rmap

-----------------------------------------------

-- | Uses ReifiedLens' to avoid impredicative polymorphism
type RefHandler m v s a b = Handler m (IORef v, ReifiedLens' v s) a b

newtype RefHandlerModeller m a b v s = RefHandlerModeller {
    runHandlerModeller :: RefHandler m v s a b
    }

type instance F.Modeller (RefHandlerModeller m a b v) s = RefHandler m v s a b

instance F.ViaModel (RefHandlerModeller m a b v) where
    viaModel l (Handler hdl) =
        Handler $ \(ref, Lens this) a -> hdl (ref, Lens (this.l)) a

-- toRefHandler :: R.MonadReactor m => Handler m s a b -> RefHandler m v s y a b
-- toRefHandler (Handler hdl) = Handler $ \(_, ref, Lens this) a -> do
--     obj <- R.doReadIORef ref
--     hdl (obj ^. this) a

toFacetedHandler :: Applicative m => Handler m r a b -> Handler m r (Which '[a]) (Which '[b])
toFacetedHandler hdl = suppressHandlerInput trial' (pickOnly <$> hdl)

-- -- | expand the types a handler handles, by '+||+' with an id handler for the extra types.
-- -- AllowAmbiguousTypes: Use TypeApplications to specify x instead of proxy.
-- bypass :: forall x a' b' m r y a b.
--     ( Monad m
--     , a' ~ Append a x
--     , b' ~ AppendUnique b x
--     , a ~ Complement a' x
--     , Reinterpret x a'
--     , Diversify b b'
--     , Diversify x b'
--     )
--     => Handler m r y (Which a) (Which b) -> Handler m r y (Which a') (Which b')
-- bypass hdl = let cid = C.id :: Handler m r y (Which x) (Which x)
--              in hdl +||+ cid

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
         P.PSemigroup (PHandler m r)
              (Which a1, Which b1)
              (Which a2, Which b2)
              (Which a3, Which b3) where
    x `pmappend` y = x +||+ y

------------------------------------------------------

-- -- | Like unix @cat@, forward input to output.
-- idHandler :: Monad m => Handler m v s a a
-- idHandler = C.id
