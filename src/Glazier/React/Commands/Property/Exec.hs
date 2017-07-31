module Glazier.React.Commands.Property.Exec where

import qualified JavaScript.Extras as JE
import Glazier.React.Commands.Property

execProperty :: PropertyCommand -> IO ()
execProperty (SetPropertyCommand j prop) = JE.setProperty prop j
