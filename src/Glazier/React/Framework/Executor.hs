{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
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

import Data.Diverse
import qualified Data.DList as DL
-- import qualified GHC.Generics as G
-- import qualified Glazier.React.Framework.Parameterized as F
import qualified Parameterized.Control.Monad as P

newtype Executor c r = Executor { runExecutor :: DL.DList c -> IO r }
    deriving Functor

instance Applicative (Executor c) where
    pure = Executor . const . pure
    (Executor f) <*> (Executor g) = Executor $ \s -> f s <*> g s

instance Monad (Executor c) where
    (Executor f) >>= k = Executor $ \s -> f s >>= k' s
      where
        k' s a = runExecutor (k a) s

-----------------------------------
-- instance F.IsPUnary (Executor c) Executor c where
--     toPUnary = id
--     fromPUnary = id

instance P.PPointed Executor (Which '[]) where
    ppure = pure

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
    papply (Executor x) (Executor y) =
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
    pbind (Executor x) k =
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

-- | Ignore certain inputs
suppressExecutor :: (a' -> Maybe a) -> Executor a r -> Executor a' r
suppressExecutor f (Executor exec) = Executor $ \as' -> exec (foldMap go as')
  where
    go a'= case f a' of
        Nothing -> mempty
        Just a -> DL.singleton a

-- ----------------------------------------

-- newtype ExecutorPNullary c = ExecutorPNullary { runExecutorPNullary :: Executor c () }
--     deriving (G.Generic)

-- instance F.IsPNullary (Executor c ()) ExecutorPNullary c where
--     toPNullary = ExecutorPNullary
--     fromPNullary = runExecutorPNullary

-- instance P.PMEmpty ExecutorPNullary (Which '[]) where
--     pmempty = ExecutorPNullary (P.ppure ())

-- -- | UndecidableInstance!
-- instance ( Reinterpret' b c
--          , Reinterpret' a c
--          , (Complement c b) ~ a
--          , (Complement c a) ~ b
--          , c ~ Append a b
--          ) =>
--          P.PSemigroup ExecutorPNullary (Which a) (Which b) (Which c) where
--     pmappend (ExecutorPNullary (Executor x)) (ExecutorPNullary (Executor y)) =
--         ExecutorPNullary . Executor $ \cs ->
--             let lcs = foldMap lgo cs
--                 rcs = foldMap rgo cs
--             in x lcs >> y rcs
--       where
--         lgo c =
--             case reinterpret' c of
--                 Nothing -> DL.empty
--                 Just c' -> DL.singleton c'
--         rgo c =
--             case reinterpret' c of
--                 Nothing -> DL.empty
--                 Just c' -> DL.singleton c'

-- instance N.Newtype (ExecutorPNullary c)

-- pmappend2 :: ( Reinterpret' b c
--          , Reinterpret' a c
--          , (Complement c b) ~ a
--          , (Complement c a) ~ b
--          , c ~ Append a b
--          ) => Executor (Which a) () -> Executor (Which b) () -> Executor (Which c) ()
-- pmappend2 x y = N.op ExecutorPNullary ((N.pack x) `P.pmappend` (N.pack y))


-- execIso :: Iso (Executor c ()) (Executor d ()) (ExecutorPNullary c) (ExecutorPNullary d)
-- execIso = iso ExecutorPNullary runExecutorPNullary

-- wack :: Executor d () -> ExecutorPNullary d
-- wack c = view execIso c

-- wack2 :: ExecutorPNullary c -> Executor c ()
-- wack2 c = review execIso c

-- pmappend3
--     :: (forall x y. Iso (Executor x ()) (Executor y ()) (ExecutorPNullary x) (ExecutorPNullary y))
--     -> (ExecutorPNullary (Which a) -> ExecutorPNullary (Which b) -> ExecutorPNullary (Which c))
--     -> Executor (Which a) ()
--     -> Executor (Which b) ()
--     -> Executor (Which c) ()
-- pmappend3 l f x y = review l (f (view l x) (view l y))

-- pmappend4
--     :: (forall x y. Iso (n x ()) (n y ()) (m x) (m y))
--     -> (m t -> m u -> m v)
--     -> n t ()
--     -> n u ()
--     -> n v ()
-- pmappend4 l f x y = review l (f (view l x) (view l y))


-- pmappend5
--     :: (forall x y. Iso (n x) (n y) (m x) (m y))
--     -> (m t -> m u -> m v)
--     -> n t
--     -> n u
--     -> n v
-- pmappend5 l f x y = review l (f (view l x) (view l y))

-- wack = pmappend5 (iso ExecutorPNullary runExecutorPNullary) P.pmappend'
