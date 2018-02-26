{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Framework.Effect.MonadHTMLElement where

import Control.Lens
import Control.Monad.IO.Class
import Data.Generics.Product
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Core as R

-- Effects from methods in https://developeR.mozilla.org/en-US/docs/Web/API/HTMLElement
class Monad m => MonadHTMLElement m where
    doFocus :: R.EventTarget -> m ()
    doBlur :: R.EventTarget -> m ()

instance MonadHTMLElement R.IOReactor where
    doFocus j = liftIO $ js_focus j
    doBlur j  = liftIO $ js_blur j


focusRef ::
    ( R.MonadReactor m
    , MonadHTMLElement m
    )
    => R.GadgetId -> R.Scene m v s -> m ()
focusRef i (R.Obj ref its) = do
    obj <- R.doReadIORef ref
    let j = obj ^. its.R.plan.field @"refs".at i
    maybe (pure ()) doFocus j

blurRef ::
    ( R.MonadReactor m
    , MonadHTMLElement m
    )
    => R.GadgetId -> R.Scene m v s -> m ()
blurRef i (R.Obj ref its) = do
    obj <- R.doReadIORef ref
    let j = obj ^. its.R.plan.field @"refs".at i
    maybe (pure ()) doBlur j

#ifdef __GHCJS__

foreign import javascript unsafe
  "if ($1 && $1['focus']) { $1['focus'](); }"
  js_focus :: R.EventTarget -> IO ()

foreign import javascript unsafe
  "if ($1 && $1['blur']) { $1['blur'](); }"
  js_blur :: R.EventTarget -> IO ()

#else

js_focus :: R.EventTarget -> IO ()
js_focus _ = pure ()

js_blur :: R.EventTarget -> IO ()
js_blur _ = pure ()

#endif
