{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Framework.Attach where

import Data.Diverse
import Data.Proxy

class Attach a b c where
    -- | Combine two things together in a '<>' Semigroup sense at both the value and type level (so the type also changes).
    -- 'aempty' is an identity with respects to '+<>+'
    (+<>+) :: a -> b -> c
    -- | Combine two things together in a '<|>' Semigroup sense at both the value and type level (so the type also changes).
    -- 'aempty' is an identity with respects to '+<|>+'
    (+<|>+) :: a -> b -> c

infixr 6 +<>+ -- like <>
infixl 3 +<|>+ -- like <|>

instance c ~ Append a b => Attach (Proxy a) (Proxy b) (Proxy c) where
    _ +<>+ _ = Proxy
    (+<|>+) = (+<>+)

class AttachId a where
    -- | The Identity for 'Attach'
    aempty :: a

instance AttachId (Proxy '[]) where
    aempty = Proxy
