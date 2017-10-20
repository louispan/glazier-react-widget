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
import Data.Diverse.Profunctor
import Data.Foldable
import Data.Profunctor
import qualified Data.DList as DL
import Glazier.React.Framework.Widget as F

newtype Handler m s a b = Handler
    { runHandler :: s -> a -> m (DL.DList b)
    }

-- | identity for 'Data.Diverse.Profunctor.+||+'
nulHandler :: Monad m => Handler m s (Which '[]) (Which '[])
nulHandler = Handler (\_ _ -> pure DL.empty)

-- | Like unix @cat@, forward input to output.
idHandler :: Monad m => Handler m s a a
idHandler = C.id

-- | Ignore certain inputs
lfilterHandler :: Applicative m => (a' -> Maybe a) -> Handler m s a b -> Handler m s a' b
lfilterHandler f (Handler hdl) = Handler $ \s a' -> case f a' of
    Nothing -> pure DL.empty
    Just a -> hdl s a

-- | Ignore certain outputs
rfilterHandler :: Applicative m => (b -> Maybe b') -> Handler m s a b -> Handler m s a b'
rfilterHandler f (Handler hdl) = Handler $ \s a -> foldr go DL.empty <$> hdl s a
  where
    go b bs = case f b of
        Nothing -> bs
        Just b' -> b' `DL.cons` bs

instance Functor m => Functor (Handler m s a) where
    fmap f (Handler hdl) = Handler $ \s a -> fmap f <$> hdl s a

instance Functor m => Profunctor (Handler m s) where
    dimap f g (Handler hdl) = Handler $ \s a -> fmap g <$> hdl s (f a)

instance Functor m => Strong (Handler m s) where
    first' (Handler hdl) = Handler $ \s (a, c) -> fmap (\b -> (b, c)) <$> hdl s a
    second' (Handler hdl) = Handler $ \s (c, a) -> fmap (\b -> (c, b)) <$> hdl s a

instance Applicative m => Choice (Handler m s) where
    left' (Handler hdl) = Handler $ \s e -> case e of
        Right c -> pure $ DL.singleton $ Right c
        Left a -> fmap Left <$> hdl s a
    right' (Handler hdl) = Handler $ \s e -> case e of
        Left c -> pure $ DL.singleton $ Left c
        Right a -> fmap Right <$> hdl s a

instance Monad m => C.Category (Handler m s) where
    id = Handler $ \_ -> pure . DL.singleton
    (Handler hdl) . (Handler hdl') = Handler $ \s a -> do
        bs <- hdl' s a
        fold <$> traverse (hdl s) (DL.toList bs)

instance Monad m => Arrow (Handler m s) where
    arr f = Handler $ \_ a -> pure $ DL.singleton (f a)
    first = first'
    second = second'

instance Monad m => ArrowChoice (Handler m s) where
    left = left'
    right = right'

newtype Handler_ModelWrapper a b m s = Handler_ModelWrapper { runHandler_ModelWrapper :: Handler m s a b }

instance F.AModelWrapper (Handler m s a b) (Handler_ModelWrapper a b) m s where
    toModelWrapper = Handler_ModelWrapper
    fromModelWrapper = runHandler_ModelWrapper

instance Monad m => F.ModelWrapper (Handler_ModelWrapper p' p) m where
    wrapModel _ g (Handler_ModelWrapper (Handler hdl)) =
        Handler_ModelWrapper . Handler $ \s a -> hdl (g s) a
    wrapMModel _ g (Handler_ModelWrapper (Handler hdl)) =
        Handler_ModelWrapper . Handler $ \s a -> do
        s' <- g s
        hdl s' a
