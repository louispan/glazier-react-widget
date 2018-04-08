module Glazier.React.Framework.Concur where

import Glazier.React.Framework.Reactor
import Glazier.React.Framework.Scene


-- | Threads forked STM TMVars commands around continuations
-- so that the Applicative instance will create forked commands in parallel
newtype Concur c a = Concur
    { runConcur ::
        -- | continuation that reads from the TMVar
        (STM a -> c)
        -> (STM a, a -> c)
    }

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
