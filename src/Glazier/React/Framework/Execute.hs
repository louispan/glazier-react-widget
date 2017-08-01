{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}

module Glazier.React.Framework.Execute where

import Control.Applicative
import Control.Monad.Trans.Maybe
import Data.Kind
import Data.Proxy
import Data.Diverse
import Data.Semigroup

newtype Execute m u acts (c :: [Type]) cmds (e :: [Type]) envs =
    Execute ( Proxy c
            , Proxy e
            , u (Which acts) -> Many envs -> Which cmds -> MaybeT m ())

andExecute
    :: Monad m
    => Execute m u acts c1 cmds e1 envs
    -> Execute m u acts c2 cmds e2 envs
    -> Execute m u acts (Append c1 c2) cmds (AppendUnique e1 e2) envs
andExecute (Execute (_, _, r)) (Execute (_, _, r')) =
    Execute (Proxy, Proxy, \u e c -> MaybeT $ do
                    x <- runMaybeT $ r u e c
                    y <- runMaybeT $ r' u e c
                    pure (x <> y))

orExecute
    :: Monad m
    => Execute m u acts c1 cmds e1 envs
    -> Execute m u acts c2 cmds e2 envs
    -> Execute m u acts (AppendUnique c1 c2) cmds (AppendUnique e1 e2) envs
orExecute (Execute (_, _, r)) (Execute (_, _, r')) =
    Execute (Proxy, Proxy, \u e c -> r u e c <|> r' u e c)

-- | Identity for 'andExecute'
ignore :: Monad m => Execute m u acts '[] cmds '[] envs
ignore = Execute (Proxy, Proxy, \_ _ _ -> pure ())


execute
    :: (Monad m, UniqueMember c cmds)
    => Proxy e -> (u (Which acts) -> Many envs -> c -> MaybeT m ()) -> Execute m u acts '[c] cmds e envs
execute pe f = Execute (Proxy, pe, \u env cmd -> case trial cmd of
                      Left _ -> pure ()
                      Right cmd' -> f u env cmd')
execute'
    :: (Monad m, UniqueMember c cmds)
    => (u (Which acts) -> c -> MaybeT m ()) -> Execute m u acts '[c] cmds e envs
execute' f = Execute (Proxy, Proxy, \u _ cmd -> case trial cmd of
                      Left _ -> pure ()
                      Right cmd' -> f u cmd')
