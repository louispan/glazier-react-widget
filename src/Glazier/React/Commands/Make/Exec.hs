{-# LANGUAGE FlexibleContexts #-}

module Glazier.React.Commands.Make.Exec where

import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Free.Church
import Data.Diverse
import qualified Glazier.React as R
import Glazier.React.Commands.Make
import qualified Pipes.Concurrent as PC

execMake
    :: UniqueMember a acts
    => TMVar Int
    -> R.ReactComponent -- for Maker
    -> PC.Output (Which acts)
    -> MakeCommand a acts
    -> IO ()
execMake muid comp output (MakeCommand mks) = do
    act <- iterM (R.execMaker muid comp output) mks
    void $ atomically $ PC.send output (pick act)
