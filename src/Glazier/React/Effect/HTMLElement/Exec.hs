{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Effect.HTMLElement.Exec where

import Control.Monad.IO.Class
import Glazier.React
import Glazier.React.Effect.HTMLElement

execFocus ::
    ( MonadIO m
    )
    => Focus -> m ()
execFocus (Focus j) = liftIO $ js_focus j

execBlur ::
    ( MonadIO m
    )
    => Blur -> m ()
execBlur (Blur j) = liftIO $ js_blur j

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
