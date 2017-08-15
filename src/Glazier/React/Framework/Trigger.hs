module Glazier.React.Framework.Trigger where

import qualified GHCJS.Types as J

type Trigger = Trigger (J.JSString, J.JSVal -> IO ())
