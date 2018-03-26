{-# LANGUAGE GADTs #-}

module Glazier.React.Effect.JavaScript where

import Control.Concurrent.STM
import qualified GHCJS.Types as J
import Glazier.React.Framework
import qualified JavaScript.Extras as JE

data SetProperty where
    SetProperty :: JE.ToJS j
        => JE.Property -> j -> SetProperty

data GetProperty next where
    GetProperty :: JE.ToJS j
        => J.JSString -> j -> (JE.JSRep -> next) -> GetProperty next

type GetProperty' w = GetProperty (States w ())

instance Functor GetProperty where
    fmap f (GetProperty a b c) = GetProperty a b (f . c)

