{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Glazier.React.Widgets.List.Run
    ( run
    ) where

import Control.Concurrent.MVar
import qualified Glazier.React.Component as R
import qualified Glazier.React.Commands.Maker.Run as C.Maker
import qualified Glazier.React.Gadgets.Render.Run as G.Render
import qualified Glazier.React.Gadgets.Dispose.Run as G.Dispose
import qualified Glazier.React.Widget as R
import Glazier.React.Widgets.List as W.List
import qualified Pipes.Concurrent as PC

run
    :: MVar Int
    -> R.ReactComponent -- for Maker
    -> PC.Output (Action k itemWidget)
    -> (k -> R.CommandOf itemWidget -> IO ()) -- command runner for the items
    -> Command k itemWidget
    -> IO ()

run _ _ _ _ (RenderCommand cmd) = G.Render.run cmd

run _ _ _ _ (DisposeCommand cmd) = G.Dispose.run cmd

run muid comp output _ (MakerCommand cmd) = C.Maker.run muid comp output cmd

run _ _ _ itemCmdRun (ListCommand cmd) = run' itemCmdRun cmd

run' :: (k -> R.CommandOf itemWidget -> IO ()) -- command runner for the items
    -> Command' k itemWidget
    -> IO ()
run' itemCmdRun (ItemCommand k itemCmd) = itemCmdRun k itemCmd
