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

newtype Execute m (c :: [Type]) cmds (e :: [Type]) envs =
    Execute ( Proxy c
            , Proxy e
            , Many envs -> Which cmds -> MaybeT m ())

andExecute
    :: Monad m
    => Execute m c1 cmds e1 envs
    -> Execute m c2 cmds e2 envs
    -> Execute m (Append c1 c2) cmds (AppendUnique e1 e2) envs
andExecute (Execute (_, _, r)) (Execute (_, _, r')) =
    Execute (Proxy, Proxy, \e c -> MaybeT $ do
                    x <- runMaybeT $ r e c
                    y <- runMaybeT $ r' e c
                    pure (x <> y))

orExecute
    :: Monad m
    => Execute m c1 cmds e1 envs
    -> Execute m c2 cmds e2 envs
    -> Execute m (AppendUnique c1 c2) cmds (AppendUnique e1 e2) envs
orExecute (Execute (_, _, r)) (Execute (_, _, r')) =
    Execute (Proxy, Proxy, \e c -> r e c <|> r' e c)

-- | Identity for 'andExecute'
ignore :: Monad m => Execute m '[] cmds '[] envs
ignore = Execute (Proxy, Proxy, \_ _ -> empty)


execute
    :: (Monad m, UniqueMember c cmds)
    => Proxy e -> (Many envs -> c -> MaybeT m ()) -> Execute m '[c] cmds e envs
execute pe f = Execute (Proxy, pe, \env cmd -> case trial cmd of
                      Left _ -> empty
                      Right cmd' -> f env cmd')
execute'
    :: (Monad m, UniqueMember c cmds)
    => (c -> MaybeT m ()) -> Execute m '[c] cmds e envs
execute' f = Execute (Proxy, Proxy, \_ cmd -> case trial cmd of
                      Left _ -> empty
                      Right cmd' -> f cmd')
