module Glazier.React.Devices.Dispose.Run
    ( run
    ) where

import qualified Control.Disposable as CD
import Glazier.React.Devices.Dispose

run :: Command -> IO ()
run (DisposeCommand x) = CD.dispose x
