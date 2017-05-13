module Glazier.React.Gadgets.Property.Run
    ( run
    ) where

import qualified JavaScript.Extras as JE
import Glazier.React.Gadgets.Property

run :: Command -> IO ()

run (SetPropertyCommand prop j) = JE.setProperty prop j
