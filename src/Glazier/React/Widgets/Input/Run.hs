module Glazier.React.Widgets.Input.Run
    ( run
    ) where

import Control.Concurrent.STM
import Control.Monad
import qualified JavaScript.Extras as JE
import qualified Pipes.Concurrent as PC
import Glazier.React.Widgets.Input

run :: PC.Output act -> Command act -> IO ()

run _ (SetPropertyCommand prop j) = JE.setProperty prop j

run output (GetPropertyCommand prop j f) = do
    v <- JE.getProperty prop j
    void $ atomically $ PC.send output (f v)
