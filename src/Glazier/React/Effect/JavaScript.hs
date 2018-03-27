{-# LANGUAGE GADTs #-}

module Glazier.React.Effect.JavaScript where

import qualified GHCJS.Types as J
import Glazier.React.Framework
import qualified JavaScript.Extras as JE

data SetProperty where
    SetProperty :: JE.ToJS j
        => JE.Property -> j -> SetProperty

data GetProperty w where
    GetProperty :: JE.ToJS j
        => J.JSString -> j -> (JE.JSRep -> States w ()) -> GetProperty w


