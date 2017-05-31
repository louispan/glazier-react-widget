module Glazier.React.Commands.Property
    ( Command(..)
    ) where

import qualified GHCJS.Types as J
import qualified JavaScript.Extras as JE

data Command = SetPropertyCommand J.JSVal JE.Property
