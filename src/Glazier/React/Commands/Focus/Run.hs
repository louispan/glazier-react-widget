{-# LANGUAGE CPP #-}

module Glazier.React.Commands.Focus.Run
    ( run
    ) where

import qualified Glazier.React as R
import Glazier.React.Commands.Focus

run :: Command -> IO ()

run (FocusCommand j) = js_focus j


#ifdef __GHCJS__

foreign import javascript unsafe
  "if ($1 && $1['focus']) { $1['focus'](); }"
  js_focus :: R.EventTarget -> IO ()

#else

js_focus :: R.EventTarget -> IO ()
js_focus _ = pure ()

#endif
