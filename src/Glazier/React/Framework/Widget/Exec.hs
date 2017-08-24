{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Glazier.React.Framework.Widget.Exec
    ( execWidget
    ) where

import qualified GHCJS.Types as J
import Glazier.React.Framework.Widget
import qualified Glazier.React.Dispose as R
import qualified JavaScript.Extras as JE
import qualified JavaScript.Object as JO

widgetSetState :: [JE.Property] -> J.JSVal -> IO ()
widgetSetState props = js_widgetSetState (JE.fromProperties props) -- trigger a react render

execWidget :: WidgetCommand -> IO ()
execWidget (RenderCommand props j) = widgetSetState props j
execWidget (DisposeCommand x) = R.runDisposable x

#ifdef __GHCJS__

foreign import javascript unsafe
  "if ($2 && $2['setState']) { $2['setState']($1); }"
  js_widgetSetState :: JO.Object -> J.JSVal -> IO ()

#else

js_widgetSetState :: JO.Object -> J.JSVal -> IO ()
js_widgetSetState _ _ = pure ()

#endif
