{-# LANGUAGE OverloadedStrings #-}

module Glazier.React.Framework.Display where

import Control.Monad.Reader
import Data.Coerce
import qualified Data.DList as D
import Data.Maybe
import Data.Semigroup
import qualified Glazier as G
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Core as F
import qualified JavaScript.Extras as JE

newtype WindowProperty = WindowProperty { runWindowProperty :: JE.Property }
type ToWindowProperties dtls plns = F.Prototype dtls plns -> D.DList WindowProperty

newtype WindowListener = WindowListener { runWindowListener :: R.Listener }
type ToWindowListeners dtls plns = F.Prototype dtls plns -> D.DList WindowListener

newtype Display dtls plns =
    Display ( ToWindowProperties dtls plns
            , ToWindowListeners dtls plns
            , Maybe ( ToWindowProperties dtls plns
                   -> ToWindowListeners dtls plns
                   -> G.WindowT (F.Prototype dtls plns) R.ReactMl ()))

instance Monoid (Display dtls plns) where
    mempty = Display (mempty, mempty, Nothing)
    mappend = (<>)

-- | If properties and listeners are combined if either or both windows are Nothing.
-- If there is a window on both sides, then a new window is created to show both window
-- with their respective set of properties/listeners, and the properties of this
-- new Display is set to mempty.
-- The new window uses a div if new properties are added to this Display.
instance Semigroup (Display dtls plns) where
    Display (ps, ls, Nothing) <> Display (ps', ls', w') =
        Display (ps <> ps', ls <> ls', w')
    Display (ps, ls, w) <> Display (ps', ls', Nothing) =
        Display (ps <> ps', ls <> ls', w)
    Display (ps, ls, Just w) <> Display (ps', ls', Just w') =
        Display
            ( mempty
            , mempty
            , Just (divWrapped (w ps ls <> w' ps' ls')))

-- | wrap with a div if there are properties and listeners
divWrapped
    :: G.WindowT (F.Prototype dtls plns) R.ReactMl ()
    -> ToWindowProperties dtls plns
    -> ToWindowListeners dtls plns
    -> G.WindowT (F.Prototype dtls plns) R.ReactMl ()
divWrapped w p l = do
    s <- ask
    let l' = coerce $ l s
        p' = coerce $ p s
    case (D.toList l', D.toList p') of
        ([], []) -> w
        _ -> lift . R.bh "div" p' l' $ G.runWindowT' w s

renderDisplay :: Display dtls plns -> G.WindowT (F.Prototype dtls plns) R.ReactMl ()
renderDisplay (Display (p, l, w)) = fromMaybe mempty $ (\w' -> w' p l) <$> w
