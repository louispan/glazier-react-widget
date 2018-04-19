{-# LANGUAGE FlexibleContexts #-}

module Glazier.React.Effect.JavaScript.Exec where

import Control.Monad.IO.Class
import Glazier.React.Effect.JavaScript
import qualified JavaScript.Extras as JE

execJavascript ::
    MonadIO m
    => (c -> m ()) -> JavaScriptCmd c -> m ()
execJavascript exec c = case c of
    SetProperty j prop -> liftIO $ JE.setProperty j prop
    GetProperty j n k -> do
        r <- liftIO $ JE.getProperty j n
        exec $ k r
