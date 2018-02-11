{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Framework.Handler where

import Control.Applicative
import Control.Arrow
import qualified Control.Category as C
import Control.Lens
import Data.Diverse.Profunctor
import qualified Data.DList as DL
import Data.Foldable
import Data.Profunctor
import Data.Semigroup
import qualified Glazier.React.Framework.IsReader as F
import qualified Glazier.React.Framework.Model as F
import qualified Glazier.React.Framework.Obj as F
import qualified Parameterized.Data.Monoid as P
import qualified Parameterized.TypeLevel as P

newtype Handler m r a b = Handler
    { runHandler :: r -> a -> m (DL.DList b)
    }

instance F.IsReader r (Handler m r a b) where
    type ReaderResult r (Handler m r a b) = a -> m (DL.DList b)
    fromReader = Handler
    toReader = runHandler

instance Applicative m => Semigroup (Handler m r a b) where
    Handler f <> Handler g = Handler $ \env a -> liftA2 (<>) (f env a) (g env a)

instance Applicative m => Monoid (Handler m r a b) where
    mempty = Handler $ \_ _ -> pure mempty
    mappend = (<>)

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
-- Ie, pretend to handle more inputs.
suppressHandlerInput :: Applicative m => (a -> Maybe a') -> Handler m r a' b -> Handler m r a b
suppressHandlerInput f (Handler hdl) = Handler $ \env a -> case f a of
    Nothing -> pure DL.empty
    Just a' -> hdl env a'

-- | Ignore certain outputs
filterHandlerOutput :: Applicative m => (b -> Maybe b') -> Handler m r a b -> Handler m r a b'
filterHandlerOutput f (Handler hdl) = Handler $ \env a -> foldMap go <$> hdl env a
  where
    go b = case f b of
        Nothing -> DL.empty
        Just b' -> DL.singleton b'

-- | map the whole output
mapHandlerOutputList :: Functor m => (DL.DList b -> DL.DList b') -> Handler m r a b -> Handler m r a b'
mapHandlerOutputList f (Handler hdl) = Handler $ \env a -> f <$> hdl env a

-----------------------------------------------

type ObjHandler m v s a b = Handler m (F.Obj v s) a b
type SceneHandler x m v s a b = Handler m (F.Scene x m v s) a b

newtype ObjHandlerOnSpec m a b v s = ObjHandlerOnSpec {
    runObjHandlerOnSpec :: ObjHandler m v s a b
    }

type instance F.OnSpec (ObjHandlerOnSpec m a b v) s = ObjHandler m v s a b

instance F.ViaSpec (ObjHandlerOnSpec m a b v) where
    viaSpec l (Handler hdl) =
        Handler $ \obj -> hdl (F.edit l obj)

-- | expand the types a handler handles, by '+||+' with an id handler for the extra types.
-- @AllowAmbiguousTypes@: Use @TypeApplications@ instead of @Proxy@ to specify @x@
bypass :: forall x a' b' m r a b.
    ( Monad m
    , ChooseBetween a x a' b x b'
    )
    => Handler m r (Which a) (Which b) -> Handler m r (Which a') (Which b')
bypass hdl = let cid = C.id :: Handler m r (Which x) (Which x)
             in hdl +||+ cid

-- | Fanout the same input to two handlers and combine then results
-- RedundantConstraint: @b3 ~ AppendUnique b1 b2@
chooseBoth :: forall m r a b1 b2 b3.
    ( Applicative m
    , Diversify b1 b3
    , Diversify b2 b3
    , b3 ~ AppendUnique b1 b2)
    => Handler m r (Which a) (Which b1)
    -> Handler m r (Which a) (Which b2)
    -> Handler m r (Which a) (Which b3)
chooseBoth x y =
    (rmap diversify x)
    <>
    (rmap diversify y)

-------------------------------------

newtype PHandler m r ab = PHandler
    { runPHandler :: Handler m r (P.At0 ab) (P.At1 ab)
    }

type instance P.PNullary (PHandler m r) (a, b) = Handler m r a b

-- | NB. This is also identity for 'Data.Diverse.Profunctor.+||+'
instance Applicative m => P.PMEmpty (PHandler m r) (Which '[], Which '[]) where
    pmempty = Handler $ \_ _ -> pure DL.empty

-- | type restricted version of 'P.pmempty' for 'Handler'
nilHandler :: Applicative m => Handler m r (Which '[]) (Which '[])
nilHandler = P.pmempty

-- | Undecidableinstances!
instance (Monad m
         , ChooseBetween a1 a2 a3 b1 b2 b3
         ) =>
         P.PSemigroup (PHandler m r)
              (Which a1, Which b1)
              (Which a2, Which b2)
              (Which a3, Which b3) where
    x `pmappend` y = x +||+ y

-- | type restricted version of 'P.pmappend' for 'Handler'
andHandler ::
    (Monad m
    , ChooseBetween a1 a2 a3 b1 b2 b3
    )
    => Handler m r (Which a1) (Which b1)
    -> Handler m r (Which a2) (Which b2)
    -> Handler m r (Which a3) (Which b3)
andHandler = P.pmappend
infixr 6 `andHandler` -- like mappend
