{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Glazier.React.Framework.Widget.Run
    ( runWidget
    ) where

import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import qualified GHCJS.Types as J
import Glazier.React.Framework.Widget
import Glazier.React.Framework.Shared as F
import qualified Glazier.React.Dispose as R
import qualified JavaScript.Extras as JE
import qualified JavaScript.Object as JO

widgetSetState :: F.Shared v i -> [JE.Property] -> J.JSVal -> IO ()
widgetSetState s props j = do
    let i = s ^. ival
        t = s ^. tmvar
        l = s ^. vlens . to runLens
    void $ atomically $ do
        v <- takeTMVar t
        putTMVar t (v & l .~ i)
    js_widgetSetState (JE.fromProperties props) j

runWidget :: WidgetCommand -> IO ()
runWidget (RenderCommand s props j) = widgetSetState s props j

runWidget (DisposeCommand x) = R.getDisposable x

#ifdef __GHCJS__

foreign import javascript unsafe
  "if ($2 && $2['setState']) { $2['setState']($1); }"
  js_widgetSetState :: JO.Object -> J.JSVal -> IO ()

#else

js_widgetSetState :: JO.Object -> J.JSVal -> IO ()
js_widgetSetState _ _ = pure ()

#endif
