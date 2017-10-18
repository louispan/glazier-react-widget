{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Framework.Handler where

import qualified Control.Category as C
import Control.Applicative
import Control.Arrow
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Foldable
import Data.Profunctor
import Data.Diverse
import qualified Data.DList as DL
import Data.Tagged
import Glazier.React.Framework.Widget as F
import Data.Coerce

newtype Handler m s a b = Handler
    { runHandler :: s -> a -> MaybeT m (DL.DList b)
    }

instance Functor m => Functor (Handler m s a) where
    fmap f (Handler hdl) = Handler $ \s a -> fmap f <$> hdl s a

instance Functor m => Profunctor (Handler m s) where
    dimap f g (Handler hdl) = Handler $ \s a -> fmap g <$> hdl s (f a)

instance Functor m => Strong (Handler m s) where
    first' (Handler hdl) = Handler $ \s (a, c) -> fmap (\b -> (b, c)) <$> hdl s a
    second' (Handler hdl) = Handler $ \s (c, a) -> fmap (\b -> (c, b)) <$> hdl s a

instance Monad m => Choice (Handler m s) where
    left' (Handler hdl) = Handler $ \s e -> case e of
        Right c -> pure $ DL.singleton $ Right c
        Left a -> fmap Left <$> hdl s a
    right' (Handler hdl) = Handler $ \s e -> case e of
        Left c -> pure $ DL.singleton $ Left c
        Right a -> fmap Right <$> hdl s a

instance Monad m => C.Category (Handler m s) where
    id = Handler $ \_ a -> pure $ DL.singleton a
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

-- | identity for 'orHandler'
ignore :: Monad m => Handler m s a b
ignore = Handler (\_ _ -> empty)

handler'
    :: (Monad m, UniqueMember a' a, UniqueMember b' b)
    => (s -> a' -> MaybeT m b') -> Handler m s (F.Whichever '[a'] a) (Whichever '[b'] b)
handler' f = Handler (\s (Tagged a) -> do
    a' <- MaybeT . pure $ trial' a
    (DL.singleton . Tagged . pick) <$> f s a')

handler
    :: (Monad m, Reinterpret' a' a, Diversify b' b)
    => (s -> Which a' -> MaybeT m (DL.DList (Which b')))
    -> Handler m s (F.Whichever a' a) (Whichever b' b)
handler f = Handler $ \s (Tagged a) -> do
    a' <- MaybeT . pure $ reinterpret' a
    fmap (Tagged . diversify) <$> f s a'

-- | The intention of this combinator is to allow combining handler for different
-- input actions together.
-- Therefore, it is will be compile error to `orHandlers` of the same input types.
-- This is to prevent accidently processing an action twice.
-- The compile error only appears at the point when 'F.runWhichever'
-- is used to extract the Which type.
-- The compile error will be due to @(Append a1 a2)@ which will not satisfy
-- @UniqueMember@ constraints in handlers.
-- NB. The use of <|> only the first handler for a particular action will be used.
orHandler
    :: Monad m
    => Handler m s (F.Whichever a1 a) (F.Whichever b1 b)
    -> Handler m s (F.Whichever a2 a) (F.Whichever b2 b)
    -> Handler m s (F.Whichever (Append a1 a2) a) (F.Whichever (AppendUnique b1 b2) b)
orHandler (Handler f) (Handler g) =
    Handler (\s (Tagged a) -> liftA2 (<|>)
                (coerce <$> f s (coerce a))
                (coerce <$> g s (coerce a)))

newtype Handler_ModelWrapper a b m s = Handler_ModelWrapper { runHandler_ModelWrapper :: Handler m s a b }

instance F.AModelWrapper (Handler m s a b) (Handler_ModelWrapper a b) m s where
    toModelWrapper = Handler_ModelWrapper
    fromModelWrapper = runHandler_ModelWrapper

instance Monad m => F.ModelWrapper (Handler_ModelWrapper p' p) m where
    wrapModel _ g (Handler_ModelWrapper (Handler hdl)) =
        Handler_ModelWrapper . Handler $ \s a -> hdl (g s) a
    wrapMModel _ g (Handler_ModelWrapper (Handler hdl)) =
        Handler_ModelWrapper . Handler $ \s a -> do
        s' <- lift (g s)
        hdl s' a
