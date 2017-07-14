module Glazier.React.Devices.Dispose.Run
    ( run
    ) where

import qualified Glazier.React.Dispose as R
import Glazier.React.Devices.Dispose

run :: Command -> IO ()
run (DisposeCommand x) = R.runDisposable x
