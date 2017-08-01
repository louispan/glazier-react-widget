module Glazier.React.Commands.Property where

import qualified GHCJS.Types as J
import qualified JavaScript.Extras as JE

data SetPropertyCommand = SetPropertyCommand J.JSVal JE.Property
