{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}

module Glazier.React.Framework.Executor where

import Control.Applicative
import Control.Monad.Trans.Maybe
import Data.Coerce
import Data.Diverse
-- import Data.Functor.Contravariant
import Data.Kind
import Data.Proxy
import Data.Semigroup (Semigroup(..))

type Executor' c = (Which c -> MaybeT IO ())

-- instance Contravariant Executor' where
--     contramap f (Executor' exec) = Executor' (exec . f)

newtype Executor (c :: [Type]) cmds =
    Executor ( Proxy c
            , Executor' cmds)

instance Semigroup (Executor '[] cmds) where
    _ <> _ = Executor (Proxy, const empty)

instance Monoid (Executor '[] cmds) where
    mempty = Executor (Proxy, const empty)
    mappend = (<>)

getExecutor :: Executor c cmds -> Executor' cmds
getExecutor (Executor (_, exec)) = exec

-- | mempty is also identity for 'orExecutor'
-- NB. Due to the use of <|> only the first handler for a particular command will be used.
-- This is to prevent running executors twice for the one command.
-- This will be compile time check with @Append c1 c2@ and @UniqueMember@ constraints.
orExecutor
    :: Executor c1 cmds
    -> Executor c2 cmds
    -> Executor (Append c1 c2) cmds
orExecutor (Executor (_, r)) (Executor (_, r')) =
    Executor (Proxy, \c -> coerce r c <|> coerce r' c)

executor
    :: (UniqueMember c cmds)
    => (c -> MaybeT IO ()) -> Executor '[c] cmds
executor f = Executor (Proxy, \cmd -> case trial' cmd of
                      Nothing -> empty
                      Just cmd' -> f cmd')

-- trivial :: UniqueMember () cmds => Executor '[()] cmds
-- trivial = executor (const $ pure ())
