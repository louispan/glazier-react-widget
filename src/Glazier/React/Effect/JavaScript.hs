{-# LANGUAGE GADTs #-}

module Glazier.React.Effect.JavaScript where

import qualified GHCJS.Types as J
import qualified JavaScript.Extras as JE

data SetProperty where
    SetProperty :: JE.ToJS j
        => JE.Property -> j -> SetProperty

data GetProperty next where
    GetProperty :: JE.ToJS j
        => J.JSString -> j -> (JE.JSRep -> next) -> GetProperty next

instance Functor GetProperty where
    fmap f (GetProperty a b c) = GetProperty a b (f . c)
