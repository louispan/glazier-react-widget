module Glazier.React.Widget.Input.Run
    ( run
    ) where

import Control.Concurrent.STM
import Control.Monad
import qualified GHCJS.Types as J
import qualified JavaScript.Extras as JE
import qualified Pipes.Concurrent as PC
import Glazier.React.Widget.Input

run :: (J.JSString -> act) -> PC.Output act -> Command act -> IO ()
run toSubmitAction output (SubmitCommand str) = void $ atomically $ PC.send output (toSubmitAction str)

run _ _ (SetPropertyCommand prop j) = JE.setProperty prop j

run _ output (GetPropertyCommand prop j f) = do
    v <- JE.getProperty prop j
    void $ atomically $ PC.send output (f v)
