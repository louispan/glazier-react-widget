{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}

module Glazier.React.Gadgets.Render.Run
    ( run
    ) where

import Control.Concurrent.MVar
import Control.Lens
import Control.Monad
import qualified GHCJS.Types as J
import qualified Glazier.React.Model as R
import Glazier.React.Gadgets.Render
import qualified JavaScript.Extras as JE
import qualified JavaScript.Object as JO

componentSetState :: R.HasGizmo giz mdl pln => giz -> [JE.Property] -> J.JSVal -> IO ()
componentSetState giz props j = do
    let scn = giz ^. R.scene
        frm = giz ^. R.frame
    void $ swapMVar frm scn
    js_componentSetState (JE.fromProperties props) j

run :: R.HasGizmo giz mdl pln => Command giz -> IO ()
run (RenderCommand giz props j) = componentSetState giz props j

#ifdef __GHCJS__

foreign import javascript unsafe
  "if ($2 && $2['setState']) { $2['setState']($1); }"
  js_componentSetState :: JO.Object -> J.JSVal -> IO ()

#else

js_componentSetState :: JO.Object -> J.JSVal -> IO ()
js_componentSetState _ _ = pure ()

#endif
