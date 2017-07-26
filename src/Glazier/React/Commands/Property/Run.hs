module Glazier.React.Commands.Property.Run where

import qualified JavaScript.Extras as JE
import Glazier.React.Commands.Property

runProperty :: PropertyCommand -> IO ()
runProperty (SetPropertyCommand j prop) = JE.setProperty prop j
