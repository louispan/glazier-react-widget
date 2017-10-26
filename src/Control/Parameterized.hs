{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeInType #-}

-- | This is a parameterized monad encoding that only uses one "parameter"
-- as opposed to Control.Monad.Indexed in package "indexed"
-- which uses two "from" and "to" parameters.
module Control.Parameterized where

import Data.Kind

-- | Parameterized version of mempty in Monoid
class PEmpty (w :: k -> Type) (t :: k) | w -> t where
    pempty' :: w t

-- | Parameterized version of (<>) in Semigroup
-- pempty is the identity for pappend
class PSemigroup (w :: k -> Type) (t :: k) (u :: k) (v :: k) | t u -> v where
    pappend' :: w t -> w u -> w v

-- | Converts a type to/from a "parameterized" type which is a monotype
-- (after application of the "parameter" type argument)
-- FunctionalDependency: @x -> w t, w t -> x@ so that it is injective both directions.
class IsPNullary (x :: Type) (w :: k -> Type) (t :: k) | x -> w t, w t -> x where
    toPNullary :: x -> w t
    fromPNullary :: w t -> x

pempty :: (IsPNullary x w t, PEmpty w t) => x
pempty = fromPNullary pempty'

pappend :: (IsPNullary x w t, IsPNullary y w u, IsPNullary z w v, PSemigroup w t u v) => x -> y -> z
pappend x y = fromPNullary (toPNullary x `pappend'` toPNullary y)

-- | Converts a type to/from a "parameterized" type which accepts one type argument
-- (after application of the "parameter" type argument)
-- FunctionalDependency: so that it is injective both directions.
class IsPUnary x (w :: k -> Type -> Type) (t :: k) | x -> w t where
    toPUnary :: x a -> w t a
    fromPUnary :: w t a -> x a

class PPointed (w :: k -> Type -> Type) (t :: k) | w -> t where
    ppure' :: a -> w t a

-- | Parameterized version of Applicative.
-- TODO: Do we need a PPointed constraint? Can't use one here as the @p@ in @PPointed w p@ is ambiguous.
class PApplicative (w :: k -> Type -> Type) (t :: k) (u :: k) (v :: k) | t u -> v where
    papply' :: w t (a -> b) -> w u a -> w v b

-- | Parameterized version of Monad.
-- TODO: Do we need a PApplicative constraint?
class PMonad (w :: k -> Type -> Type) (t :: k) (u :: k) (v :: k) | t u -> v where
    pbind' :: w t a -> (a -> w u b) -> w v b

ppure :: (IsPUnary x w t, PPointed w t) => a -> x a
ppure = fromPUnary . ppure'

papply
    :: (IsPUnary x w t, IsPUnary y w u, IsPUnary z w v, PApplicative w t u v)
    => x (a -> b) -> y a -> z b
papply x y = fromPUnary (toPUnary x `papply'` toPUnary y)

pbind
    :: (IsPUnary x w t, IsPUnary y w u, IsPUnary z w v, PMonad w t u v)
    => x a -> (a -> y b) -> z b
pbind x k = fromPUnary (toPUnary x `pbind'` (toPUnary . k))

-- | Get the first type of a type level tuple
type family Fst ab where
    Fst '(a, b) = a

-- | Get the second type of a type level tuple
type family Snd ab where
    Snd '(a, b) = b
