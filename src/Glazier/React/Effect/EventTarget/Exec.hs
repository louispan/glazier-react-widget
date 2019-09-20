{-# LANGUAGE CPP #-}

module Glazier.React.Effect.EventTarget.Exec where

import Glazier.React

execAddEventListener :: EventTarget -> JSString -> Listener -> IO ()
execAddEventListener j n cb = js_addEventListener (toJS j) n (toJS cb)

execRemoveEventListener :: EventTarget -> JSString -> Listener -> IO ()
execRemoveEventListener j n cb = js_removeEventListener (toJS j) n (toJS cb)

#ifdef __GHCJS__

foreign import javascript unsafe
    "if ($1 && $1['addEventListener']) { $1['addEventListener']($2, $3); }"
    js_addEventListener :: JSVal -> JSString -> JSVal -> IO ()

foreign import javascript unsafe
    "if ($1 && $1['removeEventListener']) { $1['removeEventListener']($2, $3); }"
    js_removeEventListener :: JSVal -> JSString -> JSVal -> IO ()

#else

js_addEventListener :: JSVal -> JSString -> JSVal -> IO ()
js_addEventListener _ _ _ = pure mempty

js_removeEventListener :: JSVal -> JSString -> JSVal -> IO ()
js_removeEventListener _ _ _ = pure mempty

#endif
