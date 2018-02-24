{-# LANGUAGE FlexibleContexts #-}

module Glazier.React.Framework.Effect.MonadJS where

import Control.Monad.IO.Class
import Data.Coerce
import qualified GHCJS.Types as J
import qualified Glazier.React as R
import qualified JavaScript.Extras as JE

class Monad m => MonadJS m where
    doSetProperty :: Coercible j J.JSVal => JE.Property -> j -> m ()
    doGetProperty :: Coercible j J.JSVal => J.JSString -> j -> m JE.JSRep

instance MonadJS R.IOReactor where
    doSetProperty prop j = liftIO $ JE.setProperty prop j
    doGetProperty n j  = liftIO $ JE.getProperty n j
