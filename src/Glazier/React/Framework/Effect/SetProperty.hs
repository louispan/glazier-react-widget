module Glazier.React.Framework.Effect.SetProperty where

import qualified GHCJS.Types as J
import qualified JavaScript.Extras as JE

data SetProperty = SetProperty JE.Property J.JSVal
