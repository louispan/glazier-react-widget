module Glazier.React.Commands.Property.Run
    ( run
    ) where

import qualified JavaScript.Extras as JE
import Glazier.React.Commands.Property

run :: Command -> IO ()

run (SetPropertyCommand j prop) = JE.setProperty prop j
