{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Framework.Executor where

import Data.Functor.Contravariant
import Data.Diverse

newtype Executor c = Executor
    { runExecutor :: c -> IO ()
    }

instance Contravariant Executor where
    contramap f (Executor exec) = Executor $ exec . f

-- | 'nulExecutor' is also identity for 'orExecutor'
nulExecutor :: Executor (Which '[])
nulExecutor = Executor . const $ pure ()

-- | Ignore certain inputs
lfilterExecutor :: (a' -> Maybe a) -> Executor a -> Executor a'
lfilterExecutor f (Executor exec) = Executor $ \a' -> case f a' of
    Nothing -> pure ()
    Just a -> exec a

-- | 'noop' is also identity for 'orExecutor'
-- | The intention of this combinator is to allow combining executors for different
-- input actions together.
-- Therefore, it is will be compile error to `orExecutor` of the same input types.
-- This is to prevent accidently processing an action twice.
-- The compile error will be due to @(Append c1 c2)@ which will not satisfy
-- @UniqueMember@ constraints.
-- NB. The use of <|> only the first executor for a particular action will be used.
orExecutor
    :: (Reinterpret c2 (Append c1 c2), (Complement (Append c1 c2) c2) ~ c1)
    => Executor (Which c1)
    -> Executor (Which c2)
    -> Executor (Which (Append c1 c2))
orExecutor (Executor x) (Executor y) =
    Executor $ \c ->
        case reinterpret c of
            Left c' -> x c'
            Right c' -> y c'
