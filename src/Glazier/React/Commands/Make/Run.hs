module Glazier.React.Commands.Make.Run where

import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Free.Church
import qualified Glazier.React as R
import Glazier.React.Commands.Make
import qualified Pipes.Concurrent as PC

runMake
    :: TMVar Int
    -> R.ReactComponent -- for Maker
    -> PC.Output a
    -> MakeCommand a
    -> IO ()
runMake muid comp output (MakeCommand mks) = do
    act <- iterM (R.runMaker muid comp output) mks
    void $ atomically $ PC.send output act
