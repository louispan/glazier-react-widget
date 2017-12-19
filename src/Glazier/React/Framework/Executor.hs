{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Framework.Executor where

-- import Data.Diverse
import qualified Data.DList as DL
import Data.Profunctor
-- import qualified Parameterized.Control.Monad as P

newtype Executor m c x = Executor { runExecutor :: DL.DList c -> m x }
    deriving Functor

instance Applicative m => Applicative (Executor m c) where
    pure = Executor . const . pure
    (Executor f) <*> (Executor g) = Executor $ \s -> f s <*> g s

instance Monad m => Monad (Executor m c) where
    (Executor f) >>= k = Executor $ \s -> f s >>= k' s
      where
        k' s x = runExecutor (k x) s

instance Functor m => Profunctor (Executor m) where
    dimap f g (Executor exec) = Executor $ \xs -> g <$> exec (f <$> xs)

-- -----------------------------------

-- type instance P.PUnary Executor c = Executor c

-- instance P.PPointed Executor (Which '[]) where
--     ppure = pure

-- -- | UndecidableInstanced!
-- -- The intention of this combinator is to allow combining executors for different
-- -- input actions together.
-- -- Therefore, it is will be compile error to `orExecutor` of the with the same input types.
-- -- This is to prevent accidently processing an action twice.
-- -- The compile error will be due to @(Append c1 c2)@ which will not satisfy
-- -- @UniqueMember@ constraints.
-- -- NB. The use of <|> only the first executor for a particular action will be used.
-- instance ( Reinterpret' b c
--          , Reinterpret' a c
--          , (Complement c b) ~ a
--          , (Complement c a) ~ b
--          , c ~ Append a b
--          ) =>
--          P.PApplicative Executor (Which a) (Which b) (Which c) where
--     papply (Executor x) (Executor y) =
--         Executor $ \cs ->
--             let lcs = foldMap lgo cs
--                 rcs = foldMap rgo cs
--             in x lcs <*> y rcs
--       where
--         lgo c =
--             case reinterpret' c of
--                 Nothing -> DL.empty
--                 Just c' -> DL.singleton c'
--         rgo c =
--             case reinterpret' c of
--                 Nothing -> DL.empty
--                 Just c' -> DL.singleton c'

-- -- | UndecidableInstanced!
-- instance ( Reinterpret' b c
--          , Reinterpret' a c
--          , a ~ Complement c b
--          , b ~ Complement c a
--          , c ~ Append a b
--          ) =>
--          P.PMonad Executor (Which a) (Which b) (Which c) where
--     pbind (Executor x) k =
--         Executor $ \cs ->
--             let lcs = foldMap lgo cs
--                 rcs = foldMap rgo cs
--             in x lcs >>= k' rcs
--       where
--         k' s a = runExecutor (k a) s
--         lgo c =
--             case reinterpret' c of
--                 Nothing -> DL.empty
--                 Just c' -> DL.singleton c'
--         rgo c =
--             case reinterpret' c of
--                 Nothing -> DL.empty
--                 Just c' -> DL.singleton c'

-- | Ignore certain commands contravariantly
suppressExecutor :: (c -> Maybe c') -> Executor m c' x  -> Executor m c x
suppressExecutor f (Executor exec) = Executor $ \xs -> exec (foldMap go xs)
  where
    go x = case f x of
        Nothing -> mempty
        Just x' -> DL.singleton x'

-- | Map a function to the commands contravariantly
contramapExecutor :: Functor m => (c -> c') -> Executor m c' x -> Executor m c x
contramapExecutor = lmap
