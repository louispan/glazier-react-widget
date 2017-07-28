module Glazier.React.Framework.Firsts where

-- | Semigroup in the alernate 'Control.Applicative.(<|>)' sense
class Firsts a where
    (<<|>>) :: a -> a -> a
