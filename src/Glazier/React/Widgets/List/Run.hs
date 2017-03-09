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
import Control.Lens -- for contramap
import Control.Monad
import Control.Monad.Free.Church
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Foldable
import qualified Glazier.React.Component as R
import qualified Glazier.React.Command.Run as R
import qualified Glazier.React.Maker.Run as R.Maker
import qualified Glazier.React.Widget as R
import Glazier.React.Widgets.List as W.List
import qualified Pipes.Concurrent as PC

run
    :: (Action key itemWidget -> act)
    -> (key -> R.WidgetCommand itemWidget -> IO ())
    -> PC.Output act
    -> R.ReactComponent
    -> Command key itemWidget
    -> IO ()

run _ _ _ _ (RenderCommand sm props j) = R.componentSetState sm props j

run _ _ _ _ (DisposeCommand x) = CD.dispose x

run mapAction _ output comp (MakerCommand mks) = do
    act <- mapAction <$> iterM (R.Maker.run (contramap mapAction output) comp) mks
    void $ atomically $ PC.send output act

run mapAction _ output _ (SendActionsCommand acts) =
    void $ runMaybeT $ traverse_ (\act -> lift $ atomically $ PC.send output (mapAction act) >>= guard) acts

run _ itemCmdRun _ _ (ItemCommand key itemCmd) = itemCmdRun key itemCmd
