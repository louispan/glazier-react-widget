{-# LANGUAGE OverloadedStrings #-}

module Glazier.React.Framework.Display where

import Control.Applicative
import Control.Concurrent.STM
import Control.Lens
import Control.Monad.Reader
import Data.Coerce
import qualified Data.DList as D
import Data.Maybe
import Data.Semigroup
import qualified Glazier as G
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Firsts as F
import qualified Glazier.React.Framework.Widget as F
import qualified JavaScript.Extras as JE

newtype Display dtls plns =
    Display ( F.Design dtls plns -> D.DList R.Listener
            , F.Design dtls plns -> D.DList JE.Property
            , Maybe (  (F.Design dtls plns -> D.DList R.Listener)
                    -> (F.Design dtls plns -> D.DList JE.Property)
                    -> G.WindowT (F.Design dtls plns) (R.ReactMlT STM) ()))

instance Monoid (Display dtls plns) where
    mempty = clear
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

instance F.Firsts (Display dtls plns) where
    Display (ls, ps, Nothing) <<|>> Display (ls', ps', w') =
        Display (ls <> ls', ps <> ps', w')
    Display (ls, ps, w) <<|>> Display (ls', ps', Nothing) =
        Display (ls <> ls', ps <> ps', w)
    Display (ls, ps, Just w) <<|>> Display (ls', ps', Just w') = divWrapped (w ls ps <|> w' ls' ps')

-- | identity for 'Monoid'
clear :: Display dtls plns
clear = Display (mempty, mempty, Nothing)

-- | Add a list of static properties to the rendered element.
hardcode :: [JE.Property] -> Display dtls plns
hardcode ps = Display (mempty, const $ D.fromList ps, Nothing)

-- | lift a 'ReactMl ()' into a 'Display'
display :: ([R.Listener] -> [JE.Property] -> G.WindowT (F.Design dtls plns) (R.ReactMlT STM) ()) -> Display dtls plns
display f = Display (mempty, mempty, Just (\l p -> do
    s <- ask
    let ls = s ^. F.widgetPlan . F.listeners
        k = s ^. F.widgetPlan . F.key
        ps = s ^. F.properties
        l' = D.toList (D.fromList ls <> coerce (l s))
        p' = D.toList (D.singleton ("key", JE.toJS' k) <> D.fromList ps <> coerce (p s))
    f l' p'))

-- | wrap with a div iff there are properties and listeners
divWrapped
    :: G.WindowT (F.Design dtls plns) (R.ReactMlT STM) ()
    -> Display dtls plns
divWrapped w = Display (mempty, mempty, Just $ \l p -> do
    s <- ask
    let p' = D.toList . coerce . p $ s
        l' = D.toList . coerce . l $ s
    case (l', p') of
        ([], []) -> w
        _ -> G.mkWindowT (R.bh "div" l' p' . G.runWindowT w))

-- | Convert a 'Display' into a @WindowT s ReactMl@
renderDisplay :: Display dtls plns -> G.WindowT (F.Design dtls plns) (R.ReactMlT STM) ()
renderDisplay (Display (l, p, w)) = fromMaybe mempty $ (\w' -> w' l p) <$> w