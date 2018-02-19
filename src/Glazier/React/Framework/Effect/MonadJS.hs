module Glazier.React.Framework.Effect.MonadJS where

import Control.Monad.IO.Class
import qualified GHCJS.Types as J
import qualified Glazier.React as R
import qualified JavaScript.Extras as JE

class Monad m => MonadJS m where
    doSetProperty :: JE.Property -> J.JSVal -> m ()
    doGetProperty :: J.JSString -> J.JSVal -> m JE.JSVar

instance MonadJS R.IOReactor where
    doSetProperty prop j = liftIO $ JE.setProperty prop j
    doGetProperty n j  = liftIO $ JE.getProperty n j
