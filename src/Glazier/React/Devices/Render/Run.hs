{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Glazier.React.Devices.Render.Run
    ( run
    ) where

import Control.Concurrent.MVar
import Control.Lens
import Control.Monad
import qualified GHCJS.Types as J
import qualified Glazier.React.Shared as R
import Glazier.React.Devices.Render
import qualified JavaScript.Extras as JE
import qualified JavaScript.Object as JO

componentSetState :: (R.Shared mdl) -> [JE.Property] -> J.JSVal -> IO ()
componentSetState s props j = do
    let mdl = s ^. R.ival
        frm = s ^. R.mvar
    void $ swapMVar frm mdl
    js_componentSetState (JE.fromProperties props) j

run :: Command (R.Shared mdl) -> IO ()
run (RenderCommand s props j) = componentSetState s props j

#ifdef __GHCJS__

foreign import javascript unsafe
  "if ($2 && $2['setState']) { $2['setState']($1); }"
  js_componentSetState :: JO.Object -> J.JSVal -> IO ()

#else

js_componentSetState :: JO.Object -> J.JSVal -> IO ()
js_componentSetState _ _ = pure ()

#endif
