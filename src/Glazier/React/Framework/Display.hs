{-# LANGUAGE OverloadedStrings #-}

module Glazier.React.Framework.Display where

import Control.Lens
import Data.Maybe
import Data.Semigroup
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Widget as F
import qualified JavaScript.Extras as JE

type ToListeners specs = F.Design specs -> [R.Listener]
type ToProperties specs = F.Design specs -> [JE.Property]
type Window m specs = ToListeners specs -> ToProperties specs
                      -> F.Design specs -> R.ReactMlT m ()

newtype Display m specs =
    Display ( ToListeners specs
            , ToProperties specs
            , Maybe (Window m specs)
            )

instance Monad m => Monoid (Display m specs) where
    mempty = Display (mempty, mempty, mempty)
    mappend = (<>)

-- | If properties and listeners are combined if either or both windows are Nothing.
-- If there is a window on both sides, then a new window is created to show both window
-- with their respective set of properties/listeners, and the properties of this
-- new Display is set to mempty.
-- The new window uses a div if new properties are added to this Display.
instance Monad m => Semigroup (Display m specs) where
    Display (ls, ps, Nothing) <> Display (ls', ps', w') =
        Display (ls <> ls', ps <> ps', w')
    Display (ls, ps, w) <> Display (ls', ps', Nothing) =
        Display (ls <> ls', ps <> ps', w)
    Display (ls, ps, Just w) <> Display (ls', ps', Just w') = divIfNeeded (w ls ps <> w' ls' ps')

-- | Add a list of static properties to the rendered element.
decorate :: [JE.Property] -> Display m specs
decorate ps = Display (mempty, const ps, Nothing)

display :: Window m specs -> Display m specs
display w = Display (mempty, mempty, Just w)

-- | Given a initial set of listeners and properties, create a display
-- that also includes the key and properties from the WidgetPlan
widgetDisplay :: Display m specs
widgetDisplay = Display (ls, ps, Nothing)
  where
    ls s = s ^. F.plan . F.listeners
    ps s = ("key", JE.toJS' $ k s) : p s
    p s = s ^. F.properties
    k s = s ^. F.plan . F.key

-- | wrap with a div iff there are properties and listeners
divIfNeeded :: Monad m => (F.Design specs -> R.ReactMlT m ()) -> Display m specs
divIfNeeded w = Display (mempty, mempty, Just go)
  where
    go l p s = do
        let p' = p s
            l' = l s
        case (l', p') of
            ([], []) -> w s
            _ -> R.bh "div" l' p' (w s)

-- | Convert a 'Display' into a @F.Design specs -> R.ReactMlT m ()@
renderDisplay :: Monad m => Display m specs -> F.Design specs -> R.ReactMlT m ()
renderDisplay (Display (l, p, w)) = fromMaybe mempty $ (\w' -> w' l p) <$> w
