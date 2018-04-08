{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Glazier.React.Effect.JavaScript where

import Data.Diverse.Lens
import qualified GHCJS.Types as J
import qualified JavaScript.Extras as JE

data SetProperty where
    SetProperty :: JE.ToJS j
        => JE.Property -> j -> SetProperty

data GetProperty c where
    GetProperty :: JE.ToJS j
        => J.JSString
        -> j
        -> (JE.JSRep -> c)
        -> GetProperty c

type AsJavascript c =
    ( AsFacet SetProperty c
    , AsFacet (GetProperty c) c
    )
