{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Framework.Executor where

import Data.Diverse
import qualified Data.DList as DL
import qualified Control.Parameterized as P

newtype Executor c r = Executor { runExecutor :: DL.DList c -> IO r }
    deriving Functor

newtype ExecutorPNullary c = ExecutorPNullary { runExecutorPNullary :: Executor c () }

instance Applicative (Executor c) where
    pure = Executor . const . pure
    (Executor f) <*> (Executor g) = Executor $ \s -> f s <*> g s

instance Monad (Executor c) where
    (Executor f) >>= k = Executor $ \s -> f s >>= k' s
      where
        k' s a = runExecutor (k a) s

instance P.IsPNullary (Executor c ()) ExecutorPNullary c where
    toPNullary = ExecutorPNullary
    fromPNullary = runExecutorPNullary

instance P.PZero ExecutorPNullary (Which '[]) where
    pempty' = ExecutorPNullary (P.ppure' ())

-- | UndecidableInstance!
instance ( Reinterpret' b c
         , Reinterpret' a c
         , (Complement c b) ~ a
         , (Complement c a) ~ b
         , c ~ Append a b
         ) =>
         P.PSemigroup ExecutorPNullary (Which a) (Which b) (Which c) where
    pappend' (ExecutorPNullary (Executor x)) (ExecutorPNullary (Executor y)) =
        ExecutorPNullary . Executor $ \cs ->
            let lcs = foldMap lgo cs
                rcs = foldMap rgo cs
            in x lcs >> y rcs
      where
        lgo c =
            case reinterpret' c of
                Nothing -> DL.empty
                Just c' -> DL.singleton c'
        rgo c =
            case reinterpret' c of
                Nothing -> DL.empty
                Just c' -> DL.singleton c'

instance P.IsPUnary (Executor c) Executor c where
    toPUnary = id
    fromPUnary = id

instance P.PPointed Executor (Which '[]) where
    ppure' = pure

-- | UndecidableInstanced!
-- The intention of this combinator is to allow combining executors for different
-- input actions together.
-- Therefore, it is will be compile error to `orExecutor` of the with the same input types.
-- This is to prevent accidently processing an action twice.
-- The compile error will be due to @(Append c1 c2)@ which will not satisfy
-- @UniqueMember@ constraints.
-- NB. The use of <|> only the first executor for a particular action will be used.
instance ( Reinterpret' b c
         , Reinterpret' a c
         , (Complement c b) ~ a
         , (Complement c a) ~ b
         , c ~ Append a b
         ) =>
         P.PApplicative Executor (Which a) (Which b) (Which c) where
    papply' (Executor x) (Executor y) =
        Executor $ \cs ->
            let lcs = foldMap lgo cs
                rcs = foldMap rgo cs
            in x lcs <*> y rcs
      where
        lgo c =
            case reinterpret' c of
                Nothing -> DL.empty
                Just c' -> DL.singleton c'
        rgo c =
            case reinterpret' c of
                Nothing -> DL.empty
                Just c' -> DL.singleton c'

-- | UndecidableInstanced!
instance ( Reinterpret' b c
         , Reinterpret' a c
         , a ~ Complement c b
         , b ~ Complement c a
         , c ~ Append a b
         ) =>
         P.PMonad Executor (Which a) (Which b) (Which c) where
    pbind' (Executor x) k =
        Executor $ \cs ->
            let lcs = foldMap lgo cs
                rcs = foldMap rgo cs
            in x lcs >>= k' rcs
      where
        k' s a = runExecutor (k a) s
        lgo c =
            case reinterpret' c of
                Nothing -> DL.empty
                Just c' -> DL.singleton c'
        rgo c =
            case reinterpret' c of
                Nothing -> DL.empty
                Just c' -> DL.singleton c'

-- instance Semigroup (Executor (Which '[])) where
--     _ <> _ = nulExecutor

-- instance Monoid (Executor (Which '[])) where
--     mempty = nulExecutor
--     mappend = (<>)

-- -- | 'nulExecutor' is also identity for 'orExecutor'
-- nulExecutor :: Executor (Which '[]) r
-- nulExecutor = Executor $ const $ pure ()

-- | Ignore certain inputs
lfilterExecutor :: (a' -> Maybe a) -> Executor a r -> Executor a' r
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
-- orExecutor
--     :: ( Reinterpret' c2 (Append c1 c2)
--        , Reinterpret' c1 (Append c1 c2)
--        , (Complement (Append c1 c2) c2) ~ c1
--        , (Complement (Append c1 c2) c1) ~ c2
--        )
--     => Executor (Which c1)
--     -> Executor (Which c2)
--     -> Executor (Which (Append c1 c2))
-- orExecutor (Executor x) (Executor y) = Executor $ \cs ->
--     let lcs = foldMap lgo cs
--         rcs = foldMap rgo cs
--     in x lcs >> y rcs
--   where
--     lgo c = case reinterpret' c of
--         Nothing -> DL.empty
--         Just c' -> DL.singleton c'
--     rgo c = case reinterpret' c of
--         Nothing -> DL.empty
--         Just c' -> DL.singleton c'
