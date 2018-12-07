{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Glazier.React.Effect.JavaScript where

import Control.Monad.Trans.Maybe
import Data.Diverse.Lens
import qualified GHCJS.Types as J
import Glazier.Command
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

maybeGetProperty ::
    ( AsJavascript c
    , MonadCommand c m
    , JE.FromJS a
    , JE.ToJS j)
    => J.JSString -> j -> MaybeT m a
maybeGetProperty n j = MaybeT . fmap JE.fromJSR . eval' $ GetProperty n j
