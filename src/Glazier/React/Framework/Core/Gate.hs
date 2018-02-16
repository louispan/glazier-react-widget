{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Framework.Core.Gate where

import Control.Arrow
import qualified Control.Category as C
import Data.Diverse.Profunctor
import Data.Profunctor
import Data.Semigroup
import qualified Parameterized.Data.Monoid as P
import qualified Parameterized.TypeLevel as P

-- | 'Gate' is a continuation where given an output handler,
-- it will return a input handler.
-- Mememonic: A digital circuit gate with inputs and outputs.
newtype Gate r a b = Gate
    { runGate :: (b -> r) -- given handlers for outputs
              -> (a -> r) -- return handler for inputs
    }

-- Given isomorphism to @r@ to @r'@, change to result of Gate to @r'@
exchange :: (r -> r') -> (r' -> r) -> Gate r a b -> Gate r' a b
exchange f g (Gate ab) = Gate $ \k a -> f (ab (g . k) a)

pinned :: r -> Gate r a b
pinned r = Gate $ \_ _ -> r

-- | Run both gates and combine the output
meld :: (r -> r -> r) -> Gate r a b -> Gate r a b -> Gate r a b
meld go (Gate f) (Gate g) = Gate $ \k a -> f k a `go` g k a

-- | Used to ignore certain inputs.
-- Ie, pretend to handle more inputs.
suppressInput :: r -> (a -> Maybe a') -> Gate r a' b -> Gate r a b
suppressInput r f (Gate g) = Gate $ \k a -> case f a of
    Nothing -> r
    Just a' -> g k a'

-- | Used to ignore certain outputs
filterOutput :: r -> (b -> Maybe b') -> Gate r a b -> Gate r a b'
filterOutput r f (Gate g) = Gate $ \k a ->
    let go b = case f b of
                Nothing -> r
                Just b' -> k b'
    in g go a

instance Functor (Gate r a) where
    fmap f (Gate ab) = Gate $ \k a -> ab (k . f) a

instance Applicative (Gate r a) where
    pure r = Gate $ \k _ -> k r
    (Gate gab) <*> (Gate ga) = Gate $ \kb x -> flip gab x $ \ab -> ga (kb . ab) x

instance Monad (Gate r a) where
    (Gate ga) >>= f = Gate $ \kb x -> flip ga x $ \a ->
        let Gate gb = f a
        in gb kb x

-- | Connect in parallel. Input is feed to both gates and the output of
-- both gates are '<>' together.
instance Semigroup r => Semigroup (Gate r a b) where
    (Gate f) <> (Gate g) = Gate $ \k a -> f k a <> g k a

instance Monoid r => Monoid (Gate r a b) where
    mempty = pinned mempty
    f `mappend` g = meld mappend f g

-- | You can contramap the input and fmap the output of a 'Gate'.
instance Profunctor (Gate r) where
    dimap f g (Gate ab) = Gate $ \k a -> ab (k . g) (f a)

-- | Additional annotation on inputs are just copied to the output.
instance Strong (Gate r) where
    first' (Gate ab) = Gate $ \k (a, c) -> ab (\b -> k (b, c)) a
    second' (Gate ab) = Gate $ \k (c, a) -> ab (\b -> k (c, b)) a

-- | Any 'Gate' can be made into a alternative gate which only used when the input type match
-- else the input is feed directly to the output.
instance Choice (Gate r) where
    left' (Gate ab) = Gate $ \k a -> case a of
                                        Left a' -> ab (k . Left) a' -- run through provided gate
                                        Right c -> k (Right c) -- else feed input directly to output
    right' (Gate ab) = Gate $ \k a -> case a of
                                         Right a' -> ab (k . Right) a' -- run through provided gate
                                         Left c -> k (Left c) -- else feed input directly to output

-- | A Gate is a category, which means you can use '>>>' to connect 'Gates' serially.
instance C.Category (Gate r) where
    id = Gate Prelude.id
    (Gate bc) . (Gate ab) = Gate $ \k a -> ab (bc k) a

-- | Additional annotation on inputs are just copied to the output.
instance Arrow (Gate r) where
    arr f = Gate (. f)
    first = first'
    second = second'

-- | Any 'Gate' can be made into a alternative gate which only used when the input type match
-- else the input is feed directly to the output.
instance ArrowChoice (Gate r) where
    left = left'
    right = right'

newtype PGate r ab = PGate
    { runPGate :: Gate r (P.At0 ab) (P.At1 ab)
    }

type instance P.PNullary (PGate r) (a, b) = Gate r a b

-- | NB. This is also identity for 'Data.Diverse.Profunctor.+||+'
instance Monoid r => P.PMEmpty (PGate r) (Which '[], Which '[]) where
    pmempty = Gate $ \_ _ -> mempty

-- | type restricted version of 'P.pmempty' for 'Gate'
nulGate :: Monoid r => Gate r (Which '[]) (Which '[])
nulGate = P.pmempty

-- | Undecidableinstances!
instance (ChooseBetween a1 a2 a3 b1 b2 b3
         ) =>
         P.PSemigroup (PGate r)
              (Which a1, Which b1)
              (Which a2, Which b2)
              (Which a3, Which b3) where
    x `pmappend` y = x +||+ y

-- | type restricted version of 'P.pmappend' for 'Gate'
orGate ::
    (ChooseBetween a1 a2 a3 b1 b2 b3
    )
    => Gate r (Which a1) (Which b1)
    -> Gate r (Which a2) (Which b2)
    -> Gate r (Which a3) (Which b3)
orGate = P.pmappend
infixr 2 `orGate` -- like +++

-- | type restricted version of 'P.pmappend' for 'Gate'
andGate ::
    ( ChooseBoth b1 b2 b3
    )
    => (r -> r -> r)
    -> Gate r a (Which b1)
    -> Gate r a (Which b2)
    -> Gate r a (Which b3)
andGate f x y = meld f (rmap diversify x) (rmap diversify y)
infixr 2 `andGate` -- like +++

-- -- | Use the given handler to transform the Executor's environment
-- -- Simple version where the handler input type must match the Executor environment type.
-- handleBeforeExecuting' ::
--     Gate r a b
--     -> Gate r x a
--     -> Gate r x b
-- handleBeforeExecuting' gab gxa = gxa >>> gab

-- -- | Use the given handler to transform the Executor's environment
-- -- Complex version where the handler input type @Which a@ doesn't have to match the Executor environment type @Which c@,
-- -- but instead just need to be a 'Injected' subset.
-- handleBeforeExecuting :: forall a b c d r x.
--     ( Injected a c b d
--     )
--     => Gate r (Which a) (Which b)
--     -> Gate r x (Which c)
--     -> Gate r x (Which d)
-- handleBeforeExecuting gab gxc =
--     let gab' = injected @_ @_ @c gab
--     in gxc >>> gab'

-- -- | expand the types a handler handles, by '+||+' with an id handler for the extra types.
-- -- @AllowAmbiguousTypes@: Use @TypeApplications@ instead of @Proxy@ to specify @x@
-- bypass :: forall x a' b' m r a b.
--     ( Monad m
--     , ChooseBetween a x a' b x b'
--     )
--     => Handler m r (Which a) (Which b) -> Handler m r (Which a') (Which b')
-- bypass hdl = let cid = C.id :: Handler m r (Which x) (Which x)
--              in hdl +||+ cid

-- -- | Fanout the same input to two handlers and combine then results
-- -- RedundantConstraint: @b3 ~ AppendUnique b1 b2@
-- andGate :: forall r a b1 b2 b3.
--     ( Semigroup r
--     , Diversify b1 b3
--     , Diversify b2 b3
--     , b3 ~ AppendUnique b1 b2)
--     => Gate r a (Which b1)
--     -> Gate r a (Which b2)
--     -> Gate r a (Which b3)
-- andGate x y =
--     (rmap diversify x)
--     <>
--     (rmap diversify y)
