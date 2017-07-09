module Glazier.React.Commands.Maker.Run
    ( run
    ) where

import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Free.Church
import qualified Glazier.React as R
import qualified Glazier.React.Maker.Run as R.Maker
import Glazier.React.Commands.Maker
import qualified Pipes.Concurrent as PC

run
    :: TMVar Int
    -> R.ReactComponent -- for Maker
    -> PC.Output a
    -> Command a
    -> IO ()

run muid comp output (MakerCommand mks) = do
    act <- iterM (R.Maker.run muid comp output) mks
    void $ atomically $ PC.send output act
