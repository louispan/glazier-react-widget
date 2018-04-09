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
import Glazier.React.Framework.Reactor
import Glazier.React.Framework.Scene

-- | Command to run 'Concur a', given a continuation
data RunConcur c where
    RunConcur ::
        Concur c a
        -> (a -> c)
        -> RunConcur c

-- | This monad is intended to be used with @ApplicativeDo@ to allow do noation
-- for composing commands that can be run concurrently.
-- The 'Applicative' instance can merge multiple commands into the internal state of @DList c@.
-- The 'Monad' instance create a 'ForkSTM' command to 'RunConcur' before continuing the bind.
-- This monad can replace usages of the 'Control.Monad.Trans.Cont' monad, by replacing usages of
-- 'Control.Monad.Trans.evalCont' with 'evalConcur' and 'Control.Monad.Trans.cont' with 'concur'.
--
-- @
-- post . evalCont . (`evalMaybeT` memptyCmd) $ do
--     start <- MaybeT . fmap JE.fromJSR . cont $ cmd' . GetProperty "selectionStart" j
--     end <- MaybeT . fmap JE.fromJSR . cont $ cmd' . GetProperty "selectionEnd" j
--     v <- MaybeT . fmap JE.fromJSR . cont $ cmd' . GetProperty "value" j
--     let (a, b) = estimateSelectionRange (J.unpack v) (J.unpack s) start end
--     pure $ cmd' @[]
--         [ cmd $ SetProperty ("value", JE.toJSR s) j
--         , cmd $ SetProperty ("selectionStart", JE.toJSR a) j
--         , cmd $ SetProperty ("selectionEnd", JE.toJSR b) j
--         ]
-- @
--
-- becomes
--
-- @
-- post . evalConcur . (`evalMaybeT` memptyCmd) $ do
--     start <- MaybeT . fmap JE.fromJSR . concur $ cmd' . GetProperty "selectionStart" j
--     end <- MaybeT . fmap JE.fromJSR . concur $ cmd' . GetProperty "selectionEnd" j
--     v <- MaybeT . fmap JE.fromJSR . concur $ cmd' . GetProperty "value" j
--     let (a, b) = estimateSelectionRange (J.unpack v) (J.unpack s) start end
--     pure $ cmd' @[]
--         [ cmd $ SetProperty ("value", JE.toJSR s) j
--         , cmd $ SetProperty ("selectionStart", JE.toJSR a) j
--         , cmd $ SetProperty ("selectionEnd", JE.toJSR b) j
--         ]
-- @
newtype Concur c a = Concur
    -- The base STM is not intended to be blocking, but may return an STM that blocks.
    { unConcur :: StatesT (DL.DList c) STM (STM a)
    }

-- | Analogous to 'Control.Monad.Trans.cont'
concur :: (AsFacet [c] c, AsFacet (ForkSTM c) c) => ((a -> c) -> c) -> Concur c a
concur r = Concur $ do
    v <- lift newEmptyTMVar
    cs <- get
    put $ cs `DL.snoc` (r (\a -> cmd' $ ForkSTM (putTMVar v a) (const memptyCmd)))
    pure $ readTMVar v

-- | Analogous to 'Control.Monad.Trans.evalCont'
evalConcur :: AsFacet (RunConcur c) c => Concur c c -> c
evalConcur k = cmd' $ RunConcur k id

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
            (\b -> cmd' $ ForkSTM (putTMVar v b) (const memptyCmd))))
        pure $ readTMVar v
