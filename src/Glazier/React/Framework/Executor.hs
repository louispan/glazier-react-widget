{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}

module Glazier.React.Framework.Executor where

import Control.Applicative
import Control.Monad.Trans.Maybe
import Data.Coerce
import Data.Diverse
import Data.Kind
import Data.Semigroup (Semigroup(..))

type Executor' m c = (Which c -> MaybeT m ())

-- instance Contravariant Executor' where
--     contramap f (Executor' exec) = Executor' (exec . f)

newtype Executor m (c :: [Type]) cmds = Executor
    { runExecutor :: Executor' m cmds
    }

instance Monad m => Semigroup (Executor m '[] cmds) where
    _ <> _ = Executor (const empty)

instance Monad m => Monoid (Executor m '[] cmds) where
    mempty = Executor (const empty)
    mappend = (<>)

-- | mempty is also identity for 'orExecutor'
-- NB. Due to the use of <|> only the first handler for a particular command will be used.
-- This is to prevent running executors twice for the one command.
-- This will be compile time check with @Append c1 c2@ and @UniqueMember@ constraints.
orExecutor
    :: Monad m => Executor m c1 cmds
    -> Executor m c2 cmds
    -> Executor m (Append c1 c2) cmds
orExecutor (Executor r) (Executor r') =
    Executor (\cs -> coerce r cs <|> coerce r' cs)

executor
    :: (Monad m, UniqueMember c cmds)
    => (c -> MaybeT m ()) -> Executor m '[c] cmds
executor f = Executor go
  where
    go cmd =
        case trial' cmd of
            Nothing -> empty
            Just cmd' -> f cmd'

-- trivial :: UniqueMember () cmds => Executor '[()] cmds
-- trivial = executor (const $ pure ())
