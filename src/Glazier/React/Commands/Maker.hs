module Glazier.React.Commands.Maker
    ( Command(..)
    ) where

import Control.Monad.Free.Church
import qualified Glazier.React.Maker as R

-- | Run a maker command and then fire the resultant action
newtype Command a = MakerCommand (F (R.Maker a) a)
