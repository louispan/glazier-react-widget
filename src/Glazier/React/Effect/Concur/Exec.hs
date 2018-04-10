{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Effect.Concur.Exec where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict
import Data.Diverse.Lens
import qualified Data.DList as DL
import Glazier.React.Effect.Concur
import Glazier.React.Framework

-- | Run the Concur STM in a forked thread because we have no control
-- on if someone put a blocking STM that will only be unblocked by a later event,
-- or if commands have been reordered.
execForkConcur ::
    ( MonadIO m
    , AsFacet [c] c
    )
    => (m () -> IO ())
    -> (c -> m ())
    -> ForkConcur c
    -> m ()
execForkConcur runExec exec (ForkConcur (Concur m) k) = liftIO $ void $ forkIO $ do
    (ma, cs) <- atomically $ runStateT m mempty
    -- It is up to exec to executes the list concurrently or not.
    liftIO . runExec $ exec (cmd' $ DL.toList cs)
    -- Now run the blocking stm
    a <- atomically ma
    let c = k a
    liftIO . runExec $ exec c
