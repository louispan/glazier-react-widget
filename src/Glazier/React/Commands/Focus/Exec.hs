{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Glazier.React.Commands.Focus.Exec
    ( execFocus
    ) where

import Control.Monad.Trans
import Data.Diverse
import qualified Glazier.React as R
import Glazier.React.Framework.Execute as F
import Glazier.React.Commands.Focus

execFocus :: UniqueMember FocusCommand cmds => F.Execute IO u acts '[FocusCommand] cmds '[] envs
execFocus = F.execute' $ \_ (FocusCommand j) -> lift $ js_focus j

#ifdef __GHCJS__

foreign import javascript unsafe
  "if ($1 && $1['focus']) { $1['focus'](); }"
  js_focus :: R.EventTarget -> IO ()

#else

js_focus :: R.EventTarget -> IO ()
js_focus _ = pure ()

#endif
