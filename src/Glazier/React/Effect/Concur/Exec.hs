{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Effect.Concur.Exec where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict
import Data.Diverse.Lens
import qualified Data.DList as DL
import Glazier.React
import Glazier.React.Effect.Concur.Internal

-- | Run the Concur IO in a forked thread because it may need to "wait" until MVars are unblocked.
execForkConcur ::
    ( MonadIO m
    , AsFacet [c] c
    )
    => (m () -> IO ())
    -> (c -> m ())
    -> ForkConcur c
    -> m ()
execForkConcur runExec exec (ForkConcur (Concur m) k) = liftIO $ void $ forkIO $ do
    -- get the list of commands to run
    (ma, cs) <- runStateT m mempty
    liftIO . runExec $ exec (cmd' $ DL.toList cs)
    -- Now run the blocking io, which produces the final command
    a <- ma
    let c = k a
    liftIO . runExec $ exec c
