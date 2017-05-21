module Glazier.React.Gadgets.Property
    ( Command(..)
    , Action(..)
    , gadget
    ) where

import Control.Lens
import Control.Monad.Reader
import qualified Data.DList as D
import qualified GHCJS.Types as J
import qualified Glazier as G
import qualified JavaScript.Extras as JE

data Command
    = SetPropertyCommand J.JSVal JE.Property

data Action
    = SetPropertyAction J.JSVal JE.Property

-- Can't use makeClassyPrisms ''Action for single constructors
class AsAction r where
    _Action :: Prism' r Action
    _SetPropertyAction :: Prism' r (J.JSVal, JE.Property)
    _SetPropertyAction = (.) _Action _SetPropertyAction

instance AsAction Action where
    _Action = id
    _SetPropertyAction =
        prism
            (\(j, s) -> SetPropertyAction j s)
            (\x ->
                 case x of
                     SetPropertyAction j s -> Right (j, s))

gadget :: G.Gadget Action s (D.DList Command)
gadget = do
    a <- ask
    case a of
        SetPropertyAction j props -> pure $ D.singleton $ SetPropertyCommand j props
