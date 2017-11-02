{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}

module Glazier.React.Framework.Parameterized where

import Data.Kind
import qualified Parameterized.Data.Monoid as P
import qualified Parameterized.Control.Monad as P

-- | Converts a type to/from a "parameterized" type which is a monotype
-- after application of the "parameter" kind argument (eg. Monoid, Semigroup)
-- This is useful when there is only one meaningful parameterized version of a type,
-- but that type may not have the type arguments in the correct order.
-- This class enables calling "parameterized" combinators on those those types.
-- Note: the functional dependencies are injective both directions to help type inference.
class IsPNullary (x :: Type) (n :: k -> Type) (t :: k) | x -> n t, n t -> x where
    toPNullary :: x -> n t
    fromPNullary :: n t -> x

-- | Converts a type to/from a "parameterized" type which accepts one type argument,
-- after application of the "parameter" kind argument (eg. Functor, Monad)
-- This is useful when there is only one meaningful parameterized version of a type,
-- but that type may not have the type arguments in the correct order.
-- This class enables calling "parameterized" combinators on those those types.
-- Note: the functional dependencies are injective both directions to help type inference.
class IsPUnary x (m :: k -> Type -> Type) (t :: k) | x -> m t, m t -> x where
    toPUnary :: x a -> m t a
    fromPUnary :: m t a -> x a

pmappend' :: (IsPNullary x n t, IsPNullary y n u, IsPNullary z n v, P.PSemigroup n t u v) => x -> y -> z
pmappend' x y = fromPNullary (toPNullary x `P.pmappend` toPNullary y)

(&<>^) :: (IsPNullary x n t, IsPNullary y n u, IsPNullary z n v, P.PSemigroup n t u v) => x -> y -> z
(&<>^) = pmappend'
infixr 6 &<>^
infixr 6 `pmappend'`

pmempty' :: (IsPNullary x n id, P.PMEmpty n id) => x
pmempty' = fromPNullary P.pmempty

-- | Lift a value into an action
ppure' :: (IsPUnary x m id, P.PPointed m id) => a -> x a
ppure' = fromPUnary . P.ppure

-- | Sequential application.
papply' :: (IsPUnary x m t, IsPUnary y m u, IsPUnary z m v, P.PApplicative m t u v) => x (a -> b) -> y a -> z b
papply' x y = fromPUnary (P.papply (toPUnary x) (toPUnary y))

-- | Sequential application.
(&<*>^) :: (IsPUnary x m t, IsPUnary y m u, IsPUnary z m v, P.PApplicative m t u v) => x (a -> b) -> y a -> z b
(&<*>^) = papply'
infixr 6 &<*>^
infixr 6 `papply'`

(&*>^) :: (IsPUnary x m t, IsPUnary y m u, IsPUnary z m v, P.PApplicative m t u v) => x a -> y b -> z b
x &*>^ y = fromPUnary $ (toPUnary x) P.&*> (toPUnary y)
infixl 4 &*>^

(&<*^) :: (IsPUnary x m t, IsPUnary y m u, IsPUnary z m v, P.PApplicative m t u v) => x a -> y b -> z a
x &<*^ y = fromPUnary $ (toPUnary x) P.&<* (toPUnary y)
infixl 4 &<*^

pliftA' :: (Functor (m t), IsPUnary x m t) => (a -> b) -> x a -> x b
pliftA' f x = fromPUnary $ f <$> toPUnary x

pliftA2' :: (Functor x, IsPUnary x m t, IsPUnary y m u, IsPUnary z m v, P.PApplicative m t u v) => (a -> b -> c) -> x a -> y b -> z c
pliftA2' f x y = (f <$> x) `papply'` y

pliftA3'
    :: ( P.PApplicative m t u v
       , P.PApplicative m v w x
       , Functor f
       , IsPUnary f m t
       , IsPUnary g m u
       , IsPUnary h m v
       , IsPUnary i m w
       , IsPUnary j m x
       )
    => (a -> b -> c -> d) -> f a -> g b -> i c -> j d
pliftA3' f a b c = pliftA2' f a b &<*>^ c

pempty' :: (IsPUnary x m id, P.PEmpty m id) => x a
pempty' = fromPUnary P.pempty

pappend'
    :: ( IsPUnary x m t
       , IsPUnary y m u
       , IsPUnary z m v
       , P.PAlternative m t u v
       )
    => x a -> y a -> z a
pappend' x y = fromPUnary $ toPUnary x `P.pappend` toPUnary y
(&<|>^)
    :: ( IsPUnary x m t
       , IsPUnary y m u
       , IsPUnary z m v
       , P.PAlternative m t u v
       )
    => x a -> y a -> z a
(&<|>^) = pappend'
infixl 3 &<|>^
infixl 3 `pappend'`
