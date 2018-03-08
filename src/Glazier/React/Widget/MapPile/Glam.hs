{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Widget.MapPile.Glam
    (
    -- * MapPile.Glam
    hdlGlamMapPileDeleteItem
    , hdlGlamMapPileInsertItem
    -- * Re-exports
    , module Glazier.React.Widget.Pile.Glam
    ) where

import Control.Lens
import Control.Monad.Reader
import Data.Generics.Product
import qualified Data.Map.Strict as M
import Glazier.React.Framework
import Glazier.React.Widget.MapPile
import Glazier.React.Widget.Pile.Glam

hdlGlamMapPileDeleteItem :: (MonadReactor m, Ord k)
    => Finalizer s m
    -> k -> MethodT (Scene p m (GlamPile ftr srt (M.Map k) s)) m ()
hdlGlamMapPileDeleteItem fin k = do
    this@Obj{..} <- ask
    lift $ lift $ do
        doModifyIORef' self (my._model.field @"glamList" .~ []) -- this tells render to update displayItems
        dirty this
    magnify (to $ accessScene (field @"rawPile")) (hdlMapPileDeleteItem fin k)

hdlGlamMapPileInsertItem :: (MonadReactor m, Ord k)
    => Finalizer s m
    -> (k, s) -> MethodT (Scene p m (GlamPile ftr srt (M.Map k) s)) m ()
hdlGlamMapPileInsertItem fin k = do
    this@Obj{..} <- ask
    lift $ lift $ do
        doModifyIORef' self (my._model.field @"glamList" .~ []) -- this tells render to update displayItems
        dirty this
    magnify (to $ accessScene (field @"rawPile")) (hdlMapPileInsertItem fin k)
