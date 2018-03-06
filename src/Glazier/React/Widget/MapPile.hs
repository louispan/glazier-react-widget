{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
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

module Glazier.React.Widget.MapPile where

import Control.Lens
import Control.Monad.Trans
import qualified Data.Map.Strict as M
import Data.Semigroup
import Glazier.React.Framework

hdlMapPileDeleteItem :: (MonadReactor m, Ord k)
    => Finalizer s m
    -> k -> Delegate (Scene p m (M.Map k s)) m ()
hdlMapPileDeleteItem fin k = delegate' $ \this@Obj{..} -> lift $ do
    me <- doReadIORef self
    let mi = M.lookup k (me ^. my._model)
    fin' <- maybe (pure mempty) (runMethod' fin) mi
    doWriteIORef self $ me
        & (my._model %~ M.delete k)
        & (my._plan._disposeOnUpdated %~ (<> fin'))
    dirty this

hdlMapPileInsertItem :: (MonadReactor m, Ord k)
    => Finalizer s m
    -> (k, s) -> Delegate (Scene p m (M.Map k s)) m ()
hdlMapPileInsertItem fin (k, s) = delegate' $ \this@Obj{..} -> lift $ do
    me <- doReadIORef self
    let mi = M.lookup k (me ^. my._model)
    fin' <- maybe (pure mempty) (runMethod' fin) mi
    doWriteIORef self $ me
        & (my._model %~ M.insert k s)
        & (my._plan._disposeOnUpdated %~ (<> fin'))
    dirty this
