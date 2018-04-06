{-# LANGUAGE FlexibleContexts #-}

module Glazier.React.Effect.JavaScript.Exec where

import Control.Monad.IO.Class
import Glazier.React.Effect.JavaScript
import qualified JavaScript.Extras as JE

execSetProperty ::
    ( MonadIO m
    )
    => SetProperty -> m ()
execSetProperty (SetProperty prop j) = liftIO $ JE.setProperty prop j

execGetProperty ::
    MonadIO m
    => (c -> m ())
    -> GetProperty c
    -> m ()
execGetProperty exec (GetProperty n j k) = do
    r <- liftIO $ JE.getProperty n j
    exec $ k r
