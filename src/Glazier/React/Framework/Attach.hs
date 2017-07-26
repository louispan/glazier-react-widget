{-# LANGUAGE MultiParamTypeClasses #-}

module Glazier.React.Framework.Attach where

class Attach a b c where
    -- | Combine two things together in a '<>' Semigroup sense at both the value and type level (so the type also changes).
    -- 'aempty' is an identity with respects to '+<>+'
    (+<>+) :: a -> b -> c
    -- | Combine two things together in a '<|>' Semigroup sense at both the value and type level (so the type also changes).
    -- 'aempty' is an identity with respects to '+<|>+'
    (+<|>+) :: a -> b -> c

infixr 6 +<>+ -- like <>
infixl 3 +<|>+ -- like <|>

class AttachId a where
    -- | The Identity for 'Attach'
    aempty :: a
