{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Glazier.React.Effect.JavaScript where

import Data.Diverse.Lens
import GHC.Stack
import qualified GHCJS.Types as J
import Glazier.Command
import Glazier.React.Reactor
import qualified JavaScript.Extras as JE

type AsJavascript c = AsFacet (JavaScriptCmd c) c

data JavaScriptCmd c where
    SetProperty :: JE.ToJS j
        => JE.Property -> j -> JavaScriptCmd c
    GetProperty :: JE.ToJS j
        => J.JSString
        -> j
        -> (JE.JSRep -> c)
        -> JavaScriptCmd c

instance Show (JavaScriptCmd c) where
    showsPrec d (SetProperty p j) = showParen (d >= 11) $
        showString "SetProperty "
        . showsPrec 11 p
        . showChar ' '
        . showsPrec 11 (JE.toJSR j)
        . showString "}"
    showsPrec d (GetProperty n j _) = showParen (d >= 11) $
        showString "GetProperty "
        . showsPrec 11 n
        . showChar ' '
        . showsPrec 11 (JE.toJSR j)
        . showString "}"

-- | get a property of any JSVal. If a null or undefined is queried, the result will also be null
getProperty ::
    ( HasCallStack
    , AsReactor c
    , AsJavascript c
    , MonadCommand c m
    , JE.ToJS j)
    => J.JSString -> j -> m JE.JSRep
getProperty n j = tracedEval' callStack $ GetProperty n j

-- | set a property of any JSVal
setProperty ::
    ( HasCallStack
    , AsReactor c
    , AsJavascript c
    , MonadCommand c m
    , JE.ToJS j
    )
    => JE.Property -> j -> m ()
setProperty p j = tracedExec' callStack $ SetProperty p j
