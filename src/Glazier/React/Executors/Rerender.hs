{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Glazier.React.Executors.Rerender where

import Glazier.React.Framework.Widget
import qualified Glazier.React.Framework.Widget as F
import qualified JavaScript.Extras as JE
import qualified JavaScript.Object as JO

execRerender :: F.Rerender -> IO ()
execRerender (Rerender j ps) = js_widgetSetState (JE.fromProperties ps) j

#ifdef __GHCJS__

foreign import javascript unsafe
  "if ($2 && $2['setState']) { $2['setState']($1); }"
  js_widgetSetState :: JO.Object -> F.ComponentRef -> IO ()

#else

js_widgetSetState :: JO.Object -> F.ComponentRef -> IO ()
js_widgetSetState _ _ = pure ()

#endif
