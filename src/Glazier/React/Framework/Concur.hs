{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Framework.Concur where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad.State
import Control.Monad.Trans.States.Strict
import Data.Diverse.Lens
import qualified Data.DList as DL
import Data.Semigroup
import Glazier.React.Framework.Reactor
import Glazier.React.Framework.Scene

-- | The base STM monad is assummed not to block
-- but it may return an STM that may block.
newtype Concur c a = Concur
    { unConcur :: StatesT (DL.DList c) STM (STM a)
    }

data RunConcur c where
    RunConcur ::
        Concur c a
        -> (a -> c)
        -> RunConcur c

concur :: (AsFacet [c] c, AsFacet (ForkSTM c) c) => ((a -> c) -> c) -> Concur c a
concur r = Concur $ do
    v <- lift newEmptyTMVar
    cs <- get
    put $ cs `DL.snoc` (r (\a -> cmd' $ ForkSTM (putTMVar v a) (const emptyCmd)))
    pure $ readTMVar v

concurAsCmd :: AsFacet (RunConcur c) c => Concur c c -> c
concurAsCmd k = cmd' $ RunConcur k id

-- addRequests :: DL.DList c -> Concur c ()
-- addRequests cs = Concur $ do
--     cs' <- get
--     put $ cs' <> cs

instance Functor (Concur c) where
    fmap f (Concur m) = Concur $ fmap f <$> m

-- | Applicative instand allows building up list of commands without blocking
instance Applicative (Concur c) where
    pure = Concur . pure . pure
    (Concur f) <*> (Concur a) = Concur $ liftA2 (<*>) f a

-- Monad instance can't build commands without blocking.
instance (AsFacet [c] c, AsFacet (ForkSTM c) c, AsFacet (RunConcur c) c) => Monad (Concur c) where
    (Concur m) >>= k = Concur $ do
        m' <- m
        cs <- get
        v <- lift newEmptyTMVar
        put $ cs `DL.snoc` (cmd' $ ForkSTM m'
            (\a -> cmd' $ RunConcur (k a)
            (\b -> cmd' $ ForkSTM (putTMVar v b) (const emptyCmd))))
        pure $ readTMVar v


-- -- | Thread forked STM TMVar around a continuation
-- concurTMVar ::
--     ( Applicative m
--     , AsFacet [c] c
--     )
--     => ((a -> c) -> c) -> Concur c a
-- concurTMVar m = ConcurT $ \fire ->
--     (newEmptyTMVar,
--     fork newEmptyTMVar $ \v ->
--         let x = m $ \a -> fork (putTMVar v a) noop
--             y = fire (readTMVar v)
--         in liftA2 (<>) x y
