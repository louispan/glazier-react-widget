{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Prototypes.Input.TextInput
    ( textInputPrototype
    ) where

import Control.Lens
import Data.Bifunctor
import Data.Diverse.Lens
import qualified Data.DList as DL
import qualified Glazier.React as R
import qualified Glazier.React.Framework as F
import qualified Glazier.React.Prototypes.Input as P
import qualified JavaScript.Extras as JE

textInputPrototype
    :: ( R.MonadReactor m
       , UniqueMember (DL.DList JE.Property) p
       , UniqueMember (DL.DList JE.Property) s
       , UniqueMember (DL.DList R.Listener) s
       )
    => F.Prototype m v
            (Many p)
            (Many s)
            (Many '[DL.DList JE.Property])
            (Many '[DL.DList JE.Property, DL.DList R.Listener])
            (Which '[])
            (Which '[])
            (Which '[P.SubmitInput, P.CancelInput])
textInputPrototype = F.mapBuilder
    (second (\s -> s & item @(DL.DList JE.Property) %~ (DL.cons ("type", "text"))))
    P.inputPrototype
