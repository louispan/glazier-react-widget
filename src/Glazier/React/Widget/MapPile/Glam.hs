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

module Glazier.React.Widget.MapPile.Glam where

import Control.Lens
import Control.Monad.Reader
import Data.Generics.Product
import qualified Data.Map.Strict as M
import Glazier.React
import Glazier.React.Framework
import Glazier.React.Widget.MapPile as W
import Glazier.React.Widget.Pile.Glam as W

hdlGlamMapPileDeleteItem :: (MonadReactor m, Ord k)
    => Finalizer s m
    -> k -> Delegate (Scene p m (W.GlamPile flt srt (M.Map k) s)) m ()
hdlGlamMapPileDeleteItem fin k = do
    this@Obj{..} <- ask
    lift $ do
        doModifyIORef' self (my._model.field @"glamList" .~ []) -- this tells render to update displayItems
        dirty this
    magnify (editScene (field @"rawPile")) (W.hdlMapPileDeleteItem fin k)

hdlGlamMapPileInsertItem :: (MonadReactor m, Ord k)
    => Finalizer s m
    -> (k, s) -> Delegate (Scene p m (W.GlamPile flt srt (M.Map k) s)) m ()
hdlGlamMapPileInsertItem fin k = do
    this@Obj{..} <- ask
    lift $ do
        doModifyIORef' self (my._model.field @"glamList" .~ []) -- this tells render to update displayItems
        dirty this
    magnify (editScene (field @"rawPile")) (W.hdlMapPileInsertItem fin k)
