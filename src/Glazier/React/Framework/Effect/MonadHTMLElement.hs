{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Framework.Effect.MonadHTMLElement where

import Control.Lens
import Control.Monad.IO.Class
import Data.Diverse.Lens
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Core as F

-- Effects from methods in https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement
class Monad m => MonadHTMLElement m where
    doFocus :: R.EventTarget -> m ()
    doBlur :: R.EventTarget -> m ()

instance MonadHTMLElement R.IOReactor where
    doFocus j = liftIO $ js_focus j
    doBlur j  = liftIO $ js_blur j


-- @AllowAmbiguousTypes@: Use @TypeApplications@ instead of @Proxy@ to specify @t@
focusRef :: forall t m v s.
    ( R.MonadReactor m
    , MonadHTMLElement m
    , HasItemTag' t R.EventTarget s
    )
    => F.Scene m v s -> m ()
focusRef (F.Obj ref its) = do
    obj <- R.doReadIORef ref
    doFocus $ obj ^. its.F.model.itemTag' @t @R.EventTarget

-- @AllowAmbiguousTypes@: Use @TypeApplications@ instead of @Proxy@ to specify @t@
blurRef :: forall t m v s.
    ( R.MonadReactor m
    , MonadHTMLElement m
    , HasItemTag' t R.EventTarget s
    )
    => F.Scene m v s -> m ()
blurRef (F.Obj ref its) = do
    obj <- R.doReadIORef ref
    doBlur $ obj ^. its.F.model.itemTag' @t @R.EventTarget

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
