module Glazier.React.Commands.Make where

import Control.Monad.Free.Church
import Data.Diverse
import qualified Glazier.React as R
import Pipes.Concurrent as PC

-- | Run a maker command
data MakeCommand a = MakeCommand (PC.Output a) (F (R.Maker a) ())
