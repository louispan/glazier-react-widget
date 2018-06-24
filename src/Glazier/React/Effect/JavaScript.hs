{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Glazier.React.Effect.JavaScript where

import Data.Diverse.Lens
import qualified GHCJS.Types as J
import qualified JavaScript.Extras as JE

type AsJavascript cmd = AsFacet (JavaScriptCmd cmd) cmd

data JavaScriptCmd cmd where
    SetProperty :: JE.ToJS j
        => j -> JE.Property -> JavaScriptCmd c
    GetProperty :: JE.ToJS j
        => j
        -> J.JSString
        -> (JE.JSRep -> cmd)
        -> JavaScriptCmd cmd

instance Show (JavaScriptCmd cmd) where
    showsPrec d (SetProperty j p) = showParen (d >= 11) $
        showString "SetProperty "
        . showsPrec 11 (JE.toJSR j)
        . showChar ' '
        . showsPrec 11 p
        . showString "}"
    showsPrec d (GetProperty j n _) = showParen (d >= 11) $
        showString "GetProperty "
        . showsPrec 11 (JE.toJSR j)
        . showChar ' '
        . showsPrec 11 n
        . showString "}"


-- FIXME: Add an operator to do a chain of GetProperty, and SetProperty?
