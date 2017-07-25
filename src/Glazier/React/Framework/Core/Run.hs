{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Glazier.React.Framework.Core.Run
    ( run
    ) where

import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import qualified GHCJS.Types as J
import Glazier.React.Framework.Core
import Glazier.React.Framework.Shared as F
import qualified Glazier.React.Dispose as R
import qualified JavaScript.Extras as JE
import qualified JavaScript.Object as JO

componentSetState :: F.Shared v i -> [JE.Property] -> J.JSVal -> IO ()
componentSetState s props j = do
    let i = s ^. ival
        t = s ^. tmvar
        l = s ^. vlens . to runLens
    void $ atomically $ do
        v <- takeTMVar t
        putTMVar t (v & l .~ i)
    js_componentSetState (JE.fromProperties props) j

run :: ComponentCommand -> IO ()
run (RenderCommand s props j) = componentSetState s props j

run (DisposeCommand x) = R.runDisposable x

#ifdef __GHCJS__

foreign import javascript unsafe
  "if ($2 && $2['setState']) { $2['setState']($1); }"
  js_componentSetState :: JO.Object -> J.JSVal -> IO ()

#else

js_componentSetState :: JO.Object -> J.JSVal -> IO ()
js_componentSetState _ _ = pure ()

#endif
