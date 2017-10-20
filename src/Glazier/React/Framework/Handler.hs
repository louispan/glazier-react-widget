{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Framework.Handler where

import qualified Control.Category as C
import Control.Arrow
import Control.Lens
import Data.Diverse.Profunctor
import Data.Foldable
import Data.IORef
import Data.Profunctor
import qualified Data.DList as DL
import Glazier.React.Framework.Widget as F

-- | Uses ReifiedLens' to avoid impredicative polymorphism
newtype Handler v s m a b = Handler
    { runHandler :: IORef v -> ReifiedLens' v s -> a -> m (DL.DList b)
    }

-- | identity for 'Data.Diverse.Profunctor.+||+'
nulHandler :: Monad m => Handler v s m (Which '[]) (Which '[])
nulHandler = Handler (\_ _ _ -> pure DL.empty)

-- | Like unix @cat@, forward input to output.
idHandler :: Monad m => Handler v s m a a
idHandler = C.id

-- | Ignore certain inputs
lfilterHandler :: Applicative m => (a' -> Maybe a) -> Handler v s m a b -> Handler v s m a' b
lfilterHandler f (Handler hdl) = Handler $ \v l a' -> case f a' of
    Nothing -> pure DL.empty
    Just a -> hdl v l a

-- | Ignore certain outputs
rfilterHandler :: Applicative m => (b -> Maybe b') -> Handler v s m a b -> Handler v s m a b'
rfilterHandler f (Handler hdl) = Handler $ \v l a -> foldr go DL.empty <$> hdl v l a
  where
    go b bs = case f b of
        Nothing -> bs
        Just b' -> b' `DL.cons` bs

instance Functor m => Functor (Handler v s m a) where
    fmap f (Handler hdl) = Handler $ \v l a -> fmap f <$> hdl v l a

instance Functor m => Profunctor (Handler v s m) where
    dimap f g (Handler hdl) = Handler $ \v l a -> fmap g <$> hdl v l (f a)

instance Functor m => Strong (Handler v s m) where
    first' (Handler hdl) = Handler $ \v l (a, c) -> fmap (\b -> (b, c)) <$> hdl v l a
    second' (Handler hdl) = Handler $ \v l (c, a) -> fmap (\b -> (c, b)) <$> hdl v l a

instance Applicative m => Choice (Handler v s m) where
    left' (Handler hdl) = Handler $ \v l e -> case e of
        Right c -> pure $ DL.singleton $ Right c
        Left a -> fmap Left <$> hdl v l a
    right' (Handler hdl) = Handler $ \v l e -> case e of
        Left c -> pure $ DL.singleton $ Left c
        Right a -> fmap Right <$> hdl v l a

instance Monad m => C.Category (Handler v s m) where
    id = Handler $ \_ _ -> pure . DL.singleton
    (Handler hdl) . (Handler hdl') = Handler $ \v l a -> do
        bs <- hdl' v l a
        fold <$> traverse (hdl v l) (DL.toList bs)

instance Monad m => Arrow (Handler v s m) where
    arr f = rmap f C.id
    first = first'
    second = second'

instance Monad m => ArrowChoice (Handler v s m) where
    left = left'
    right = right'

newtype Handler_Modeller v a b m s = Handler_Modeller { runHandler_Modeller :: Handler v s m a b }

instance F.Modeller (Handler v s m a b) (Handler_Modeller v a b m) s where
    toModeller = Handler_Modeller
    fromModeller = runHandler_Modeller

instance Monad m => F.ViaModel (Handler_Modeller v a b m) where
    viaModel l' (Handler_Modeller (Handler hdl)) =
        Handler_Modeller . Handler $ \v (Lens l) a -> hdl v (Lens (l . l')) a
