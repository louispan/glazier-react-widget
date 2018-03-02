{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Framework.Effect.MonadHTMLElement where

import Control.Lens
import Control.Monad.IO.Class
import Data.Generics.Product
import qualified Glazier.React as Z
import qualified Glazier.React.Framework.Core as Z

-- Effects from methods in https://developeR.mozilla.org/en-US/docs/Web/API/HTMLElement
class Monad m => MonadHTMLElement m where
    doFocus :: Z.EventTarget -> m ()
    doBlur :: Z.EventTarget -> m ()

instance MonadHTMLElement Z.IOReactor where
    doFocus j = liftIO $ js_focus j
    doBlur j  = liftIO $ js_blur j


focusRef ::
    ( Z.MonadReactor m
    , MonadHTMLElement m
    )
    => Z.GadgetId -> Z.Scene m v s -> m ()
focusRef i (Z.Obj ref its) = do
    obj <- Z.doReadIORef ref
    let j = obj ^. its.Z.plan.field @"refs".at i
    maybe (pure ()) doFocus j

blurRef ::
    ( Z.MonadReactor m
    , MonadHTMLElement m
    )
    => Z.GadgetId -> Z.Scene m v s -> m ()
blurRef i (Z.Obj ref its) = do
    obj <- Z.doReadIORef ref
    let j = obj ^. its.Z.plan.field @"refs".at i
    maybe (pure ()) doBlur j

#ifdef __GHCJS__

foreign import javascript unsafe
  "if ($1 && $1['focus']) { $1['focus'](); }"
  js_focus :: Z.EventTarget -> IO ()

foreign import javascript unsafe
  "if ($1 && $1['blur']) { $1['blur'](); }"
  js_blur :: Z.EventTarget -> IO ()

#else

js_focus :: Z.EventTarget -> IO ()
js_focus _ = pure ()

js_blur :: Z.EventTarget -> IO ()
js_blur _ = pure ()

#endif
