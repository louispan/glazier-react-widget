{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Prototypes.Input.TextInput where

import Control.Lens
import Data.Bifunctor
import Data.Diverse.Lens
import qualified Data.DList as DL
import qualified Glazier.React as R
import qualified Glazier.React.Framework as F
import qualified Glazier.React.Prototypes.Input as P
import qualified JavaScript.Extras as JE

textInput
    :: ( R.MonadReactor x m
       , HasItem' (DL.DList JE.Property) p
       , HasItem' (DL.DList JE.Property) s
       , HasItem' (DL.DList R.Listener) s
       )
    => F.Prototype m v p s
            (Many '[DL.DList JE.Property])
            (Many '[DL.DList JE.Property, DL.DList R.Listener])
            x
            (Which '[P.SubmitInput, P.CancelInput])
            (Which '[])
            (Which '[])
textInput = F.mapBuilder
    (second (\s -> s & item' @(DL.DList JE.Property) %~ (DL.cons ("type", "text"))))
    P.input
