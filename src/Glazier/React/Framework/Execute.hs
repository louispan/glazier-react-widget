{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}

module Glazier.React.Framework.Execute where

import Data.Kind
import Data.Proxy
import Data.Diverse
import Data.Semigroup

newtype Execute m (c :: [Type]) cmds (e :: [Type]) envs =
    Execute ( Proxy c
            , Proxy e
            , Many envs -> Which cmds -> m ())

andExecute
    :: Applicative m
    => Execute m c1 cmds e1 envs
    -> Execute m c2 cmds e2 envs
    -> Execute m (AppendUnique c1 c2) cmds (AppendUnique e1 e2) envs
andExecute (Execute (_, _, r)) (Execute (_, _, r')) = Execute (Proxy, Proxy, \e c -> r e c *> r' e c)

instance Applicative m => Semigroup (Execute m c cmds e envs) where
    (Execute (_, _, r)) <> (Execute (_, _, r')) = Execute (Proxy, Proxy, \e c -> r e c *> r' e c)

-- | Identity for 'andExecute'
ignore :: Applicative m => Execute m '[] cmds '[] envs
ignore = Execute (Proxy, Proxy, \_ _ -> pure ())

execute :: (Applicative m, UniqueMember c cmds, Select e envs) => (Many e -> c -> m ()) -> Execute m '[c] cmds e envs
execute f = Execute (Proxy, Proxy, \env cmd -> case trial cmd of
                      Left _ -> pure ()
                      Right cmd' -> f (select env) cmd')
