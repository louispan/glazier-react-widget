{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Framework.Attach where

import Data.Diverse
import Data.Proxy

class Attach a b c where
    (+<>+) :: a -> b -> c
    (+<|>+) :: a -> b -> c

infixr 6 +<>+ -- like <>
infixl 3 +<|>+ -- like <|>

instance c ~ Append a b => Attach (Proxy a) (Proxy b) (Proxy c) where
    _ +<>+ _ = Proxy
    (+<|>+) = (+<>+)
