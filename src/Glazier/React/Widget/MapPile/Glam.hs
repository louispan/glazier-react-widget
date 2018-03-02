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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Widget.MapPile.Glam where

import Control.Lens
import Control.Monad.Trans
import Data.Diverse.Profunctor
import Data.Generics.Product
import qualified Data.Map.Strict as M
import qualified Glazier.React as Z
import qualified Glazier.React.Framework as Z
import qualified Glazier.React.Widget.MapPile as W
import qualified Glazier.React.Widget.Pile.Glam as W

hdlGlamMapPileDeleteItem :: (Z.MonadReactor m, Ord k)
    => Z.Finalizer m s
    -> Z.SceneHandler m v (W.GlamPile flt srt (M.Map k) s) k (Which '[])
hdlGlamMapPileDeleteItem fin this@(Z.Obj ref its) k = do
    lift $ do
        Z.doModifyIORef' ref (its.Z.model.field @"glamList" .~ []) -- this tells render to update displayItems
        Z.dirty this
    (Z.magnifyScene (field @"rawPile") (W.hdlMapPileDeleteItem fin)) this k

hdlGlamMapPileInsertItem :: (Z.MonadReactor m, Ord k)
    => Z.Finalizer m s
    -> Z.SceneHandler m v (W.GlamPile flt srt (M.Map k) s) (k, s) (Which '[])
hdlGlamMapPileInsertItem fin this@(Z.Obj ref its) k = do
    lift $ do
        Z.doModifyIORef' ref (its.Z.model.field @"glamList" .~ []) -- this tells render to update displayItems
        Z.dirty this
    (Z.magnifyScene (field @"rawPile") (W.hdlMapPileInsertItem fin)) this k
