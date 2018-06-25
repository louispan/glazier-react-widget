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

type AsJavascript cmd = AsFacet (JavaScriptCmd cmd) cmd

data JavaScriptCmd cmd where
    SetProperty :: JE.ToJS j
        => JE.Property -> j -> JavaScriptCmd c
    GetProperty :: JE.ToJS j
        => J.JSString
        -> j
        -> (JE.JSRep -> cmd)
        -> JavaScriptCmd cmd

instance Show (JavaScriptCmd cmd) where
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
    ( AsJavascript cmd
    , MonadCommand cmd m
    , JE.FromJS a
    , JE.ToJS j)
    => J.JSString -> j -> MaybeT m a
maybeGetProperty n j = MaybeT . fmap JE.fromJSR . eval' $ GetProperty n j

-- doGetProperty ::
--     ( AsJavascript cmd
--     , MonadCommand cmd m
--     , JE.ToJS j)
--     => J.JSString -> j -> m JE.JSRep
-- doGetProperty n j = outcome $ postCmd' . GetProperty n j

-- doSetProperty ::
--     ( AsJavascript cmd
--     , MonadCommand cmd m
--     , JE.ToJS j)
--     => JE.Property -> j -> m ()
-- doSetProperty prop j = postCmd' $ SetProperty prop j
