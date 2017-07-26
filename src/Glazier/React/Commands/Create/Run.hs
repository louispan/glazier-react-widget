module Glazier.React.Commands.Create.Run where

import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Free.Church
import qualified Glazier.React as R
import Glazier.React.Commands.Create
import qualified Pipes.Concurrent as PC

runCreate
    :: TMVar Int
    -> R.ReactComponent -- for Maker
    -> PC.Output a
    -> CreateCommand a
    -> IO ()
runCreate muid comp output (CreateCommand mks) = do
    act <- iterM (R.runMaker muid comp output) mks
    void $ atomically $ PC.send output act
