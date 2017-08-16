module Glazier.React.Commands.SetProperty where

import qualified GHCJS.Types as J
import qualified JavaScript.Extras as JE

data SetProperty = SetProperty J.JSVal JE.Property
