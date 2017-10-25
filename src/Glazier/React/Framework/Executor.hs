{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Glazier.React.Framework.Executor where

import Data.Diverse
import qualified Data.DList as DL
import Data.Semigroup

newtype Executor c = Executor { runExecutor :: DL.DList c -> IO () }

instance Semigroup (Executor (Which '[])) where
    _ <> _ = nulExecutor

instance Monoid (Executor (Which '[])) where
    mempty = nulExecutor
    mappend = (<>)

-- | 'nulExecutor' is also identity for 'orExecutor'
nulExecutor :: Executor (Which '[])
nulExecutor = Executor $ const $ pure ()

-- | Ignore certain inputs
lfilterExecutor :: (a' -> Maybe a) -> Executor a -> Executor a'
lfilterExecutor f (Executor exec) = Executor $ \as' -> exec (foldMap go as')
  where
    go a'= case f a' of
        Nothing -> mempty
        Just a -> DL.singleton a

-- | The intention of this combinator is to allow combining executors for different
-- input actions together.
-- Therefore, it is will be compile error to `orExecutor` of the with the same input types.
-- This is to prevent accidently processing an action twice.
-- The compile error will be due to @(Append c1 c2)@ which will not satisfy
-- @UniqueMember@ constraints.
-- NB. The use of <|> only the first executor for a particular action will be used.
orExecutor
    :: ( Reinterpret' c2 (Append c1 c2)
       , Reinterpret' c1 (Append c1 c2)
       , (Complement (Append c1 c2) c2) ~ c1
       , (Complement (Append c1 c2) c1) ~ c2
       )
    => Executor (Which c1)
    -> Executor (Which c2)
    -> Executor (Which (Append c1 c2))
orExecutor (Executor x) (Executor y) = Executor $ \cs ->
    let lcs = foldMap lgo cs
        rcs = foldMap rgo cs
    in x lcs >> y rcs
  where
    lgo c = case reinterpret' c of
        Nothing -> DL.empty
        Just c' -> DL.singleton c'
    rgo c = case reinterpret' c of
        Nothing -> DL.empty
        Just c' -> DL.singleton c'
