{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Glazier.React.Effect.JavaScript where

import Data.Diverse.Lens
import qualified GHCJS.Types as J
import qualified JavaScript.Extras as JE

type JavascriptCmds c =
    '[ [c]
    , SetProperty
    , GetProperty c
    ]

type AsJavascript c =
    ( AsFacet SetProperty c
    , AsFacet (GetProperty c) c
    )

data SetProperty where
    SetProperty :: JE.ToJS j
        => JE.Property -> j -> SetProperty

instance Show SetProperty where
    showsPrec d (SetProperty p j) = showParen (d >= 11) $
        showString "SetProperty "
        . showsPrec 11 p
        . showChar ' '
        . showsPrec 11 (JE.toJSR j)
        . showString "}"

data GetProperty c where
    GetProperty :: JE.ToJS j
        => J.JSString
        -> j
        -> (JE.JSRep -> c)
        -> GetProperty c

instance Show (GetProperty c) where
    showsPrec d (GetProperty n j _) = showParen (d >= 11) $
        showString "GetProperty "
        . showsPrec 11 n
        . showChar ' '
        . showsPrec 11 (JE.toJSR j)
        . showString "}"
