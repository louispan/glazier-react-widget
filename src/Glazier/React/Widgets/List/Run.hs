{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Glazier.React.Widgets.List.Run
    ( run
    ) where

import Control.Concurrent.STM
import qualified Control.Disposable as CD
import Control.Monad
import Control.Monad.Free.Church
import qualified Glazier.React.Component as R
import qualified Glazier.React.Command.Run as R
import qualified Glazier.React.Maker.Run as R.Maker
import qualified Glazier.React.Widget as R
import Glazier.React.Widgets.List as W.List
import qualified Pipes.Concurrent as PC

run
    :: (key -> R.CommandOf itemWidget -> IO ()) -- command runner for the items
    -> R.ReactComponent -- for Maker
    -> PC.Output (Action key itemWidget)
    -> Command key itemWidget
    -> IO ()

run _ _ _ (RenderCommand sm props j) = R.componentSetState sm props j

run _ _ _ (DisposeCommand x) = CD.dispose x

run _ comp output (MakerCommand mks) = do
    act <- iterM (R.Maker.run comp output) mks
    void $ atomically $ PC.send output act

run itemCmdRun _ _ (ItemCommand key itemCmd) = itemCmdRun key itemCmd
