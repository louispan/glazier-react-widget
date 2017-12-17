{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Prototypes.Input where

import Control.DeepSeq
import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Maybe
import qualified Control.Monad.Trans.Maybe.Extras as Ex
import Data.Diverse.Lens
import qualified Data.DList as DL
import qualified Data.JSString as J
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

input
    :: ( R.MonadReactor m
       , HasItem' (DL.DList JE.Property) p
       , HasItem' (DL.DList JE.Property) s
       , HasItem' (DL.DList R.Listener) s
       )
    => F.Prototype m v p s
            (Many '[DL.DList JE.Property])
            (Many '[DL.DList JE.Property, DL.DList R.Listener])
            (Which '[])
            (Which '[])
            (Which '[])
            (Which '[SubmitInput, CancelInput])
input =
    F.Prototype
        ( F.build @(DL.DList JE.Property)
            `P.pmappend` F.hardcode @(DL.DList R.Listener) DL.empty
        , P.pmempty
        , F.triggersRefActivator' [F.Trigger ("onKeyDown", Ex.fromMaybeT . (A.fireKeyDownKey >=> go))]
        , F.Display $ \ss -> R.lf "input"
            (DL.toList $ ss ^. item' @(DL.DList R.Listener))
            (DL.toList $ ss ^. item' @(DL.DList JE.Property))
        )
  where
    go (A.KeyDownKey target k) =
        case k of
            "Escape" -> pure . pick $ CancelInput target
            "Enter" -> do
                v <- MaybeT $ JE.fromJS' <$> JE.getProperty "value" (JE.toJS target)
                pure . pick $ SubmitInput target v
            _ -> empty
