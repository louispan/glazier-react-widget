{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Glazier.React.Effect.JavaScript where

import qualified GHCJS.Types as J
import qualified JavaScript.Extras as JE

data JavaScriptCmd c where
    SetProperty :: JE.ToJS j
        => j -> JE.Property -> JavaScriptCmd c
    GetProperty :: JE.ToJS j
        => j
        -> J.JSString
        -> (JE.JSRep -> c)
        -> JavaScriptCmd c

instance Show (JavaScriptCmd c) where
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
