module Glazier.React.Gadgets.Dispose.Run
    ( run
    ) where

import qualified Control.Disposable as CD
import Glazier.React.Gadgets.Dispose

run :: Command -> IO ()
run (DisposeCommand x) = CD.dispose x
