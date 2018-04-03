{-# LANGUAGE GADTs #-}

module Glazier.React.Effect.JavaScript where

import Control.Concurrent.STM
import qualified GHCJS.Types as J
import Glazier.React.Framework
import qualified JavaScript.Extras as JE

data SetProperty where
    SetProperty :: JE.ToJS j
        => JE.Property -> j -> SetProperty

data GetProperty c where
    GetProperty :: JE.ToJS j
        => TVar Plan
        -> TVar s
        -> J.JSString
        -> j
        -> (JE.JSRep -> States (Scenario c s) ())
        -> GetProperty c


