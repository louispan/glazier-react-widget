{-# LANGUAGE OverloadedStrings #-}

module Glazier.React.Framework.Display where

import Control.Lens
import Control.Monad.Reader
import Data.Coerce
import qualified Data.DList as D
import Data.Maybe
import Data.Semigroup
import qualified Glazier as G
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Widget as F
import qualified JavaScript.Extras as JE

newtype WindowProperty = WindowProperty JE.Property
type ToWindowProperties dtls plns = F.Design dtls plns -> D.DList WindowProperty

newtype WindowListener = WindowListener R.Listener
type ToWindowListeners dtls plns = F.Design dtls plns -> D.DList WindowListener

newtype Display dtls plns =
    Display { getDisplay :: ( ToWindowListeners dtls plns
                            , ToWindowProperties dtls plns
                            , Maybe (  ToWindowListeners dtls plns
                                    -> ToWindowProperties dtls plns
                                    -> G.WindowT (F.Design dtls plns) R.ReactMl ()))
            }

instance Monoid (Display dtls plns) where
    mempty = blank
    mappend = (<>)

-- | If properties and listeners are combined if either or both windows are Nothing.
-- If there is a window on both sides, then a new window is created to show both window
-- with their respective set of properties/listeners, and the properties of this
-- new Display is set to mempty.
-- The new window uses a div if new properties are added to this Display.
instance Semigroup (Display dtls plns) where
    Display (ls, ps, Nothing) <> Display (ls', ps', w') =
        Display (ls <> ls', ps <> ps', w')
    Display (ls, ps, w) <> Display (ls', ps', Nothing) =
        Display (ls <> ls', ps <> ps', w)
    Display (ls, ps, Just w) <> Display (ls', ps', Just w') = divWrapped (w ls ps <> w' ls' ps')

-- | identity for 'Monoid'
blank :: Display dtls plns
blank = Display (mempty, mempty, Nothing)

-- | Add a list of static properties to the rendered element.
hardcode :: [WindowProperty] -> Display dtls plns
hardcode ps = Display (mempty, const $ D.fromList ps, Nothing)

-- | lift a 'ReactMl ()' into a 'Display'
toDisplay :: ([R.Listener] -> [JE.Property] -> R.ReactMl ()) -> Display dtls plns
toDisplay f = Display (mempty, mempty, Just (\l p -> do
    s <- ask
    let ls = s ^. F.widgetPlan . F.listeners
        k = s ^. F.widgetPlan . F.key
        ps = s ^. F.properties
        l' = D.toList (D.fromList ls <> coerce (l s))
        p' = D.toList (D.singleton ("key", JE.toJS' k) <> D.fromList ps <> coerce (p s))
    lift $ f l' p'))

-- | wrap with a div iff there are properties and listeners
divWrapped
    :: G.WindowT (F.Design dtls plns) R.ReactMl ()
    -> Display dtls plns
divWrapped w = Display (mempty, mempty, Just $ \l p -> do
    s <- ask
    let p' = D.toList . coerce . p $ s
        l' = D.toList . coerce . l $ s
    case (l', p') of
        ([], []) -> w
        _ -> lift . R.bh "div" l' p' $ G.runWindowT w s)

-- | Convert a 'Display' into a @WindowT s ReactMl@
renderDisplay :: Display dtls plns -> G.WindowT (F.Design dtls plns) R.ReactMl ()
renderDisplay (Display (l, p, w)) = fromMaybe mempty $ (\w' -> w' l p) <$> w
