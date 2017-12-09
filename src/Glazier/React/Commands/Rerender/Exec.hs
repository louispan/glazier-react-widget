{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Glazier.React.Commands.Rerender.Exec where

import Glazier.React.Commands.Rerender
import qualified Glazier.React as R
import qualified JavaScript.Extras as JE
import qualified JavaScript.Object as JO

execRerender :: Rerender -> IO ()
execRerender (Rerender j ps) = js_widgetSetState (JE.fromProperties ps) j

#ifdef __GHCJS__

foreign import javascript unsafe
  "if ($2 && $2['setState']) { $2['setState']($1); }"
  js_widgetSetState :: JO.Object -> R.ReactComponent -> IO ()

#else

js_widgetSetState :: JO.Object -> R.ReactComponent -> IO ()
js_widgetSetState _ _ = pure ()

#endif
