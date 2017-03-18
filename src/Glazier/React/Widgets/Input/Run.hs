module Glazier.React.Widgets.Input.Run
    ( run
    ) where

import qualified JavaScript.Extras as JE
import Glazier.React.Widgets.Input

run :: Command -> IO ()

run (SetPropertyCommand prop j) = JE.setProperty prop j
