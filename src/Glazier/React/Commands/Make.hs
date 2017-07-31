module Glazier.React.Commands.Make
    ( MakeCommand(..)
    ) where

import Control.Monad.Free.Church
import Data.Diverse
import qualified Glazier.React as R

-- | Run a maker command and then fire the resultant action
newtype MakeCommand a acts = MakeCommand (F (R.Maker (Which acts)) a)
