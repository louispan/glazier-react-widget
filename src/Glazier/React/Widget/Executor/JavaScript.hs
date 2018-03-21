{-# LANGUAGE FlexibleContexts #-}

module Glazier.React.Widget.Executor.JavaScript where

import Control.Monad.IO.Class
import qualified GHCJS.Types as J
import Glazier.React
import qualified JavaScript.Extras as JE

class Monad m => MonadJS m where
    doSetProperty :: JE.ToJS j => JE.Property -> j -> m ()
    doGetProperty :: JE.ToJS j => J.JSString -> j -> m JE.JSRep

instance MonadJS IOReactor where
    doSetProperty prop j = liftIO $ JE.setProperty prop j
    doGetProperty n j = liftIO $ JE.getProperty n j

SetProperty