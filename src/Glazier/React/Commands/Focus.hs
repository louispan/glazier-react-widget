module Glazier.React.Commands.Focus
    ( Command(..)
    ) where

import qualified Glazier.React as R

data Command = FocusCommand R.EventTarget
