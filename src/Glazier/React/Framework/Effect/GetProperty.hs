module Glazier.React.Framework.Effect.GetProperty where

import qualified GHCJS.Types as J
import qualified JavaScript.Extras as JE

data GetProperty m = GetProperty J.JSString J.JSVal (JE.JSVar -> m ())
