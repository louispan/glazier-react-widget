{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}

module Glazier.React.Framework.Executor where

import Control.Applicative
import Control.Monad.Trans.Maybe
import Data.Kind
import Data.Proxy
import Data.Diverse
import Data.Semigroup

newtype Executor m (c :: [Type]) cmds (e :: [Type]) envs =
    Executor ( Proxy c
            , Proxy e
            , Many envs -> Which cmds -> MaybeT m ())

andExecutor
    :: Monad m
    => Executor m c1 cmds e1 envs
    -> Executor m c2 cmds e2 envs
    -> Executor m (Append c1 c2) cmds (AppendUnique e1 e2) envs
andExecutor (Executor (_, _, r)) (Executor (_, _, r')) =
    Executor (Proxy, Proxy, \e c -> MaybeT $ do
                    x <- runMaybeT $ r e c
                    y <- runMaybeT $ r' e c
                    pure (x <> y))

orExecutor
    :: Monad m
    => Executor m c1 cmds e1 envs
    -> Executor m c2 cmds e2 envs
    -> Executor m (AppendUnique c1 c2) cmds (AppendUnique e1 e2) envs
orExecutor (Executor (_, _, r)) (Executor (_, _, r')) =
    Executor (Proxy, Proxy, \e c -> r e c <|> r' e c)

-- | Identity for 'andExecutor'
ignore :: Monad m => Executor m '[] cmds '[] envs
ignore = Executor (Proxy, Proxy, \_ _ -> empty)


executor
    :: (Monad m, UniqueMember c cmds)
    => Proxy e -> (Many envs -> c -> MaybeT m ()) -> Executor m '[c] cmds e envs
executor pe f = Executor (Proxy, pe, \env cmd -> case trial cmd of
                      Left _ -> empty
                      Right cmd' -> f env cmd')
executor'
    :: (Monad m, UniqueMember c cmds)
    => (c -> MaybeT m ()) -> Executor m '[c] cmds e envs
executor' f = Executor (Proxy, Proxy, \_ cmd -> case trial cmd of
                      Left _ -> empty
                      Right cmd' -> f cmd')
