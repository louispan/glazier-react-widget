{-# LANGUAGE FlexibleContexts #-}

module Glazier.React.Effect.JavaScript.Exec where

import Control.Monad.IO.Class
import Glazier.React.Effect.JavaScript
import qualified JavaScript.Extras as JE

execJavascript ::
    MonadIO m
    => (c -> m ()) -> JavaScriptCmd c -> m ()
execJavascript executor c = case c of
    SetProperty prop j -> liftIO $ JE.setPropertyIO prop j
    GetProperty n j k -> do
        r <- liftIO $ JE.getPropertyIO n j
        executor $ k r
