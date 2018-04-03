{-# LANGUAGE FlexibleContexts #-}

module Glazier.React.Effect.JavaScript.Exec where

import Control.Concurrent.STM
import Control.Monad.IO.Class
import qualified Data.DList as DL
import Glazier.React.Effect.JavaScript
import Glazier.React.Framework
import qualified JavaScript.Extras as JE

execSetProperty ::
    ( MonadIO m
    )
    => SetProperty -> m ()
execSetProperty (SetProperty prop j) = liftIO $ JE.setProperty prop j

execGetProperty ::
    MonadIO m
    => (DL.DList c -> m ())
    -> GetProperty c
    -> m ()
execGetProperty exec (GetProperty planVar modelVar n j k) = do
    r <- liftIO $ JE.getProperty n j
    -- Apply to result to the world state, and execute any produced commands
    xs <- liftIO $ atomically $ tickState planVar modelVar (k r)
    exec xs
