{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Gadgets.Property
    ( Command(..)
    , Action(..)
    , gadget
    ) where

import Control.Monad.Reader
import qualified Data.DList as D
import qualified GHCJS.Types as J
import qualified Glazier as G
import qualified JavaScript.Extras as JE

data Command
    = SetPropertyCommand J.JSVal JE.Property

data Action
    = SetPropertyAction J.JSVal JE.Property

-- | State update logic.
-- The best practice is to leave this in general Monad m (eg, not MonadIO).
-- This allows gadget to use STM as the base monad which allows for combining concurrently
-- with other stateful STM effects and still maintain a single source of truth.
gadget :: G.Gadget Action () s (D.DList Command)
gadget = do
    a <- ask
    case a of
        SetPropertyAction j props -> pure $ D.singleton $ SetPropertyCommand j props
