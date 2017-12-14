{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Prototypes.Input where

import Control.DeepSeq
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Maybe
import qualified Control.Monad.Trans.Maybe.Extras as Ex
import Data.Diverse
import qualified Data.DList as DL
import qualified Data.JSString as J
import Data.Proxy
import qualified GHC.Generics as G
import qualified Glazier.React as R
import qualified Glazier.React.Framework as F
import qualified Glazier.React.Actions as A
import qualified JavaScript.Extras as JE
import qualified Parameterized.Data.Monoid as P

data SubmitInput = SubmitInput R.EventTarget J.JSString
    deriving (G.Generic, NFData)
data CancelInput = CancelInput R.EventTarget
    deriving (G.Generic, NFData)

inputPrototype
    :: ( R.MonadReactor m
       , UniqueMember (DL.DList JE.Property) ps
       , UniqueMember (DL.DList JE.Property) ss
       , UniqueMember (DL.DList R.Listener) ss
       )
    => F.Prototype m v
            (Many ps)
            (Many ss)
            (Many '[DL.DList JE.Property])
            (Many '[DL.DList JE.Property, DL.DList R.Listener])
            (Which '[])
            (Which '[])
            (Which '[])
            (Which '[SubmitInput, CancelInput])
inputPrototype =
    F.Prototype
        ( F.build @(DL.DList JE.Property) Proxy
            `P.pmappend` F.hardcode @(DL.DList R.Listener) DL.empty
        , P.pmempty
        , F.triggersRefActivator' [F.Trigger ("onKeyDown", Ex.fromMaybeT . (A.fireKeyDownKey >=> go))]
        , F.Display $ \ss -> R.lf "input"
            (DL.toList $ fetch @(DL.DList R.Listener) ss)
            (DL.toList $ fetch @(DL.DList JE.Property) ss)
        )
  where
    go (A.KeyDownKey target k) =
        case k of
            "Escape" -> pure . pick $ CancelInput target
            "Enter" -> do
                v <- MaybeT $ JE.fromJS' <$> JE.getProperty "value" (JE.toJS target)
                pure . pick $ SubmitInput target v
            _ -> empty
