{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Effect.Concur.Internal where

import Control.Applicative
import Control.Concurrent
import Control.Monad.State.Strict
import Data.Diverse.Lens
import qualified Data.DList as DL
import Glazier.React

-- | Command to run 'Concur a', given a continuation
data ConcurCmd c where
    ForkConcur ::
        Concur c a
        -> (a -> c)
        -> ConcurCmd c

instance Show (ConcurCmd c) where
    showsPrec d _ = showParen (d >= 11) $
        showString "ForkConcur"

-- | This monad is intended to be used with @ApplicativeDo@ to allow do noation
-- for composing commands that can be run concurrently.
-- The 'Applicative' instance can merge multiple commands into the internal state of @DList c@.
-- The 'Monad' instance create a 'ForkSTM' command to 'ForkConcur' before continuing the bind.
-- This monad can replace usages of the 'Control.Monad.Trans.Cont' monad, by replacing usages of
-- 'Control.Monad.Trans.evalCont' with 'evalConcur' and 'Control.Monad.Trans.cont' with 'concur'.
--
-- @
-- evalCont . (`evalMaybeT` (cmd @[] [])) $ do
--     start <- MaybeT . fmap JE.fromJSR . cont $ cmd' . GetProperty "selectionStart" j
--     end <- MaybeT . fmap JE.fromJSR . cont $ cmd' . GetProperty "selectionEnd" j
--     v <- MaybeT . fmap JE.fromJSR . cont $ cmd' . GetProperty "value" j
--     let (a, b) = estimateSelectionRange (J.unpack v) (J.unpack s) start end
--     pure $ cmds
--         [ cmd $ SetProperty ("value", JE.toJSR s) j
--         , cmd $ SetProperty ("selectionStart", JE.toJSR a) j
--         , cmd $ SetProperty ("selectionEnd", JE.toJSR b) j
--         ]
-- @
--
-- becomes
--
-- @
-- evalConcur . (`evalMaybeT` (cmd @[] [])) $ do
--     start <- MaybeT . fmap JE.fromJSR . concur $ cmd' . GetProperty "selectionStart" j
--     end <- MaybeT . fmap JE.fromJSR . concur $ cmd' . GetProperty "selectionEnd" j
--     v <- MaybeT . fmap JE.fromJSR . concur $ cmd' . GetProperty "value" j
--     let (a, b) = estimateSelectionRange (J.unpack v) (J.unpack s) start end
--     pure $ cmds
--         [ cmd $ SetProperty ("value", JE.toJSR s) j
--         , cmd $ SetProperty ("selectionStart", JE.toJSR a) j
--         , cmd $ SetProperty ("selectionEnd", JE.toJSR b) j
--         ]
-- @
newtype Concur c a = Concur
    -- The base IO is not intended to be blocking, but may return an IO that blocks.
    { unConcur :: StateT (DL.DList c) IO (IO a)
    }

instance Functor (Concur c) where
    fmap f (Concur m) = Concur $ fmap f <$> m

-- | Applicative instand allows building up list of commands without blocking
instance Applicative (Concur c) where
    pure = Concur . pure . pure
    (Concur f) <*> (Concur a) = Concur $ liftA2 (<*>) f a

-- Monad instance can't build commands without blocking.
instance (AsFacet [c] c, AsFacet (ConcurCmd c) c) => Monad (Concur c) where
    (Concur m) >>= k = Concur $ do
        m' <- m
        v <- lift newEmptyMVar
        cs <- get
        put $ cs `DL.snoc` (cmd' $ ForkConcur (Concur $ pure m')
            (\a -> cmd' $ ForkConcur (k a)
            (\b -> cmd' $ ForkConcur (Concur $ pure $ putMVar v b) (const $ cmd' @[] []))))
        pure $ takeMVar v

-- | Analogous to 'Control.Monad.Trans.cont'
concur :: (AsFacet [c] c, AsFacet (ConcurCmd c) c) => ((a -> c) -> c) -> Concur c a
concur r = Concur $ do
    v <- lift newEmptyMVar
    cs <- get
    put $ cs `DL.snoc` (r (\a -> cmd' $ ForkConcur (Concur $ pure $ putMVar v a) (const $ cmd' @[] [])))
    pure $ takeMVar v

-- | Analogous to 'Control.Monad.Trans.evalCont'
evalConcur :: AsFacet (ConcurCmd c) c => Concur c c -> c
evalConcur k = cmd' $ ForkConcur k id
