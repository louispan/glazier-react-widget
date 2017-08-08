module Glazier.React.Commands.Glaze where

import Control.Monad.Free.Church
import qualified Glazier.React as R
import Pipes.Concurrent as PC

-- | Run a Glaze command
data GlazeCommand a = GlazeCommand (PC.Output a) (F (R.Glaze a) ())
