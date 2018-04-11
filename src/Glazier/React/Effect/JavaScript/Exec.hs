{-# LANGUAGE FlexibleContexts #-}

module Glazier.React.Effect.JavaScript.Exec where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Glazier.React.Effect.JavaScript
import Glazier.React.Framework
import qualified JavaScript.Extras as JE

execJavascript ::
    ( MonadIO m
    , AsJavascript c
    )
    => (c -> m ()) -> c -> MaybeT m ()
execJavascript exec c =
   maybeExec execSetProperty c
    <|> maybeExec (execGetProperty exec) c

execSetProperty ::
    ( MonadIO m
    )
    => SetProperty -> m ()
execSetProperty (SetProperty j prop) = liftIO $ JE.setProperty j prop

execGetProperty ::
    MonadIO m
    => (c -> m ())
    -> GetProperty c
    -> m ()
execGetProperty exec (GetProperty j n k) = do
    r <- liftIO $ JE.getProperty j n
    exec $ k r
