module Glazier.React.Commands.Reactor where

import Control.Monad.Free.Church
import qualified Glazier.React as R
import Pipes.Concurrent as PC

-- | Run a Reactor command
data ReactorCommand a = ReactorCommand (PC.Output a) (F (R.Reactor a) ())
