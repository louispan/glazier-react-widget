module Glazier.React.Commands.Create
    ( CreateCommand(..)
    ) where

import Control.Monad.Free.Church
import qualified Glazier.React as R

-- | Run a maker command and then fire the resultant action
newtype CreateCommand a = CreateCommand (F (R.Maker a) a)
