{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}

module Glazier.React.Framework.Execute where

import Data.Kind
import Data.Proxy
import Data.Diverse
import Data.Semigroup

newtype Execute m output acts (c :: [Type]) cmds (e :: [Type]) envs =
    Execute ( Proxy c
            , Proxy e
            , output (Which acts) -> Many envs -> Which cmds -> m ())

andExecute
    :: Applicative m
    => Execute m output acts c1 cmds e1 envs
    -> Execute m output acts c2 cmds e2 envs
    -> Execute m output acts (AppendUnique c1 c2) cmds (AppendUnique e1 e2) envs
andExecute (Execute (_, _, r)) (Execute (_, _, r')) =
    Execute (Proxy, Proxy, \output e c -> r output e c *> r' output e c)

instance Applicative m =>
         Semigroup (Execute m output acts c cmds e envs) where
    (Execute (_, _, r)) <> (Execute (_, _, r')) =
        Execute (Proxy, Proxy, \output e c -> r output e c *> r' output e c)

-- | Identity for 'andExecute'
ignore :: Applicative m => Execute m output acts '[] cmds '[] envs
ignore = Execute (Proxy, Proxy, \_ _ _ -> pure ())

execute
    :: (Applicative m, UniqueMember c cmds, Select e envs)
    => (output (Which acts) -> Many e -> c -> m ()) -> Execute m output acts '[c] cmds e envs
execute f = Execute (Proxy, Proxy, \output env cmd -> case trial cmd of
                      Left _ -> pure ()
                      Right cmd' -> f output (select env) cmd')
