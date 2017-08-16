{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Glazier.React.Commands.FocusElement.Exec
    ( execFocusElement
    ) where

import qualified Glazier.React as R
import Glazier.React.Commands.FocusElement

execFocusElement :: FocusElement -> IO ()
execFocusElement (FocusElement j) = js_focus j

#ifdef __GHCJS__

foreign import javascript unsafe
  "if ($1 && $1['focus']) { $1['focus'](); }"
  js_focus :: R.EventTarget -> IO ()

#else

js_focus :: R.EventTarget -> IO ()
js_focus _ = pure ()

#endif
