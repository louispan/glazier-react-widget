{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Effect.HTMLElement.Exec where

import Control.Monad.IO.Class
import Glazier.React
import Glazier.React.Effect.HTMLElement

execHTMLElementCmd ::
    ( MonadIO m
    )
    => HTMLElementCmd -> m ()
execHTMLElementCmd c = case c of
    Blur j -> liftIO $ js_blur j
    Focus j -> liftIO $ js_focus j

#ifdef __GHCJS__

foreign import javascript unsafe
  "if ($1 && $1['focus']) { $1['focus'](); }"
  js_focus :: EventTarget -> IO ()

foreign import javascript unsafe
  "if ($1 && $1['blur']) { $1['blur'](); }"
  js_blur :: EventTarget -> IO ()

#else

js_focus :: EventTarget -> IO ()
js_focus _ = pure ()

js_blur :: EventTarget -> IO ()
js_blur _ = pure ()

#endif
