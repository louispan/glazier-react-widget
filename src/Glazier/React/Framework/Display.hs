{-# LANGUAGE OverloadedStrings #-}

module Glazier.React.Framework.Display where

import Control.Concurrent.STM
import Control.Lens
import Data.Maybe
import Data.Semigroup
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Widget as F
import qualified JavaScript.Extras as JE

type ToListeners dtls plns = F.Design dtls plns -> [R.Listener]
type ToProperties dtls plns = F.Design dtls plns -> [JE.Property]
type Window dtls plns = ToListeners dtls plns -> ToProperties dtls plns
                      -> F.Design dtls plns -> R.ReactMlT STM ()

newtype Display dtls plns =
    Display ( ToListeners dtls plns
            , ToProperties dtls plns
            , Maybe (Window dtls plns)
            )

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
    Display (ls, ps, Just w) <> Display (ls', ps', Just w') = divIfNeeded (w ls ps <> w' ls' ps')

-- | identity for 'Monoid'
blank :: Display dtls plns
blank = Display (mempty, mempty, Nothing)

-- | Add a list of static properties to the rendered element.
decorate :: [JE.Property] -> Display dtls plns
decorate ps = Display (mempty, const ps, Nothing)

windowed :: Window dtls plns -> Display dtls plns
windowed w = Display (mempty, mempty, Just w)

-- | Given a initial set of listeners and properties, create a display
-- that also includes the key and properties from the WidgetPlan
widgetDisplay :: Display dtls plns
widgetDisplay = Display (ls, ps, Nothing)
  where
    ls s = s ^. F.widgetPlan . F.listeners
    ps s = ("key", JE.toJS' $ k s) : p s
    p s = s ^. F.properties
    k s = s ^. F.widgetPlan . F.key

-- | wrap with a div iff there are properties and listeners
divIfNeeded :: (F.Design dtls plns -> R.ReactMlT STM ()) -> Display dtls plns
divIfNeeded w = Display (mempty, mempty, Just go)
  where
    go l p s = do
        let p' = p s
            l' = l s
        case (l', p') of
            ([], []) -> w s
            _ -> R.bh "div" l' p' (w s)

-- | Convert a 'Display' into a @F.Design dtls plns -> R.ReactMlT STM ()@
renderDisplay :: Display dtls plns -> F.Design dtls plns -> R.ReactMlT STM ()
renderDisplay (Display (l, p, w)) = fromMaybe mempty $ (\w' -> w' l p) <$> w
