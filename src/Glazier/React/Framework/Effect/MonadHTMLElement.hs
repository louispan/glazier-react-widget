{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Framework.Effect.MonadHTMLElement where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Glazier.React
import Glazier.React.Framework.Core
import qualified JavaScript.Extras as JE

-- Effects from methods in https://developeR.mozilla.org/en-US/docs/Web/API/HTMLElement
class Monad m => MonadHTMLElement m where
    doFocus :: EventTarget -> m ()
    doBlur :: EventTarget -> m ()

instance MonadHTMLElement IOReactor where
    doFocus j = liftIO $ js_focus j
    doBlur j  = liftIO $ js_blur j


focusRef ::
    ( MonadReactor m
    , MonadHTMLElement m
    )
    => GadgetId -> Scene p m s -> m ()
focusRef i Obj{..} = do
    me <- doReadIORef self
    void $ runMaybeT $ do
        et <- MaybeT $ pure $ do
            j <- me ^. my._plan._refs.at i
            JE.fromJSR j
        lift $ doFocus et

blurRef ::
    ( MonadReactor m
    , MonadHTMLElement m
    )
    => GadgetId -> Scene p m s -> m ()
blurRef i Obj{..} = do
    me <- doReadIORef self
    void $ runMaybeT $ do
        et <- MaybeT $ pure $ do
            j <- me ^. my._plan._refs.at i
            JE.fromJSR j
        lift $ doBlur et

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
