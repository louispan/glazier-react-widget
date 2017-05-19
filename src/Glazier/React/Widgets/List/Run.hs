{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Glazier.React.Widgets.List.Run
    ( run
    ) where

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Free.Church
import qualified Glazier.React.Component as R
import qualified Glazier.React.Gadgets.Render.Run as G.Render
import qualified Glazier.React.Gadgets.Dispose.Run as G.Dispose
import qualified Glazier.React.Maker.Run as R.Maker
import qualified Glazier.React.Widget as R
import Glazier.React.Widgets.List as W.List
import qualified Pipes.Concurrent as PC

run
    :: (k -> R.CommandOf itemWidget -> IO ()) -- command runner for the items
    -> MVar Int
    -> R.ReactComponent -- for Maker
    -> PC.Output (Action k itemWidget)
    -> Command k itemWidget
    -> IO ()

run _ _ _ _ (RenderCommand cmd) = G.Render.run cmd

run _ _ _ _ (DisposeCommand cmd) = G.Dispose.run cmd

run _ muid comp output (MakerCommand mks) = do
    act <- iterM (R.Maker.run muid comp output) mks
    void $ atomically $ PC.send output act

run itemCmdRun _ _ _ (ItemCommand k itemCmd) = itemCmdRun k itemCmd
