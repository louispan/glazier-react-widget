{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Widget.MapPile where

import Control.Lens
import Control.Monad.Trans
import Data.Diverse.Profunctor
import Data.Generics.Product
import qualified Data.Map.Strict as M
import Data.Semigroup
import qualified Glazier.React.Framework as Z

hdlMapPileDeleteItem :: (Z.MonadReactor m, Ord k)
    => Z.Finalizer m s
    -> Z.SceneHandler m v (M.Map k s) k (Which '[])
hdlMapPileDeleteItem fin this@(Z.Obj ref its) k = Z.terminate' . lift $ do
    obj <- Z.doReadIORef ref
    let mi = M.lookup k (obj ^. its.Z.model)
    fin' <- maybe (pure mempty) fin mi
    Z.doWriteIORef ref $ obj
        & (its.Z.model %~ M.delete k)
        & (its.Z.plan.field @"disposeOnUpdated" %~ (<> fin'))
    Z.dirty this

hdlMapPileInsertItem :: (Z.MonadReactor m, Ord k)
    => Z.Finalizer m s
    -> Z.SceneHandler m v (M.Map k s) (k, s) (Which '[])
hdlMapPileInsertItem fin this@(Z.Obj ref its) (k, s) = Z.terminate' . lift $ do
    obj <- Z.doReadIORef ref
    let mi = M.lookup k (obj ^. its.Z.model)
    fin' <- maybe (pure mempty) fin mi
    Z.doWriteIORef ref $ obj
        & (its.Z.model %~ M.insert k s)
        & (its.Z.plan.field @"disposeOnUpdated" %~ (<> fin'))
    Z.dirty this
