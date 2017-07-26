module Glazier.React.Commands.Property
    ( PropertyCommand(..)
    ) where

import qualified GHCJS.Types as J
import qualified JavaScript.Extras as JE

data PropertyCommand = SetPropertyCommand J.JSVal JE.Property
