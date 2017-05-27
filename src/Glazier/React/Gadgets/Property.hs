module Glazier.React.Gadgets.Property
    ( Command(..)
    , Action(..)
    , gadget
    ) where

import Control.Monad.Reader
import qualified Data.DList as D
import qualified GHCJS.Types as J
import qualified Glazier as G
import qualified JavaScript.Extras as JE

data Command = SetPropertyCommand J.JSVal JE.Property

data Action = SetPropertyAction J.JSVal JE.Property

gadget :: G.Gadget Action s (D.DList Command)
gadget = do
    a <- ask
    case a of
        SetPropertyAction j props -> pure $ D.singleton $ SetPropertyCommand j props
