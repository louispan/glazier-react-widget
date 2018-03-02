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

module Glazier.React.Widget.Mapping (
    -- * Mapping
      Mapping(..)
    , listingBuilder
    , listing
    , listingMakeItem
    , hdlListingDeleteItem
    , hdlListingSort
    , hdlListingFilter
    , hdlListingInsertItem
    , broadcastListingHandler
    ) where

import Control.Lens
import qualified Control.Monad.ListM as LM
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Data.Diverse.Profunctor
import Data.Foldable
import Data.Generics.Product
import qualified Data.Map.Strict as M
import Data.Semigroup
import qualified GHC.Generics as G
import qualified Glazier.React as Z
import qualified Glazier.React.Framework.Core as Z

-- | A listing is actually a map so that we allow for fast insert/deleting
-- and also be able to reorder the elements.
-- Assumption: Once a key is assigned to an item, it is never changed, so the item can use it's key in a callback.
data Mapping flt srt k i = Mapping
    { displayFilter :: flt
    , displaySort :: srt
    , displayList :: [i] -- filtered and sorted
    , items :: M.Map k i
    } deriving (G.Generic, Functor)

-- | Converts a builder with a plan of @[a]@ to a plan of @Mapping a@
listingBuilder :: (Applicative m)
    => Z.Builder m i s i s
    -> Z.Builder m (Mapping flt srt k i) (Mapping flt srt k s) (Mapping flt srt k i) (Mapping flt srt k s)
listingBuilder (Z.Builder (Z.MkReq mkReq, Z.MkSpec mkSpc)) =
    Z.Builder (Z.MkReq mkReq', Z.MkSpec mkSpc')
  where
    mkReq' (Mapping df ds dss ss) = Mapping df ds <$> traverse mkReq dss <*> traverse mkReq ss
    mkSpc' (Mapping df ds dps ps) = Mapping df ds <$> traverse mkSpc dps <*> traverse mkSpc ps

-- | This version drops the original item handlers @a -> b@, and only have list handlers.
listing :: (Z.MonadReactor m)
    => (flt -> s -> m Bool)
    -> (srt -> s -> s -> m Ordering)
    -> Z.Archetype m s c
    -> Z.Prototype m v (Mapping flt srt k s) c
listing flt srt (Z.Archetype dis fin ini)
    = Z.Prototype
        (listingDisplay flt srt dis)
        (\s -> fold <$> traverse fin (s ^. field @"items"))
        (listingInitializer ini)

hdlListingDeleteItem :: (Z.MonadReactor m, Ord k)
    => Z.Finalizer m s
    -> Z.SceneHandler m v (Mapping flt srt k s)
        k (Which '[])
hdlListingDeleteItem fin this@(Z.Obj ref its) k = Z.terminate' . lift $ do
    Z.doModifyIORefM ref $ \obj -> do
        let mi = M.lookup k (obj ^. its.Z.model.field @"items")
        fin' <- maybe (pure mempty) fin mi
        pure $ obj & (its.Z.model.field @"items" %~ M.delete k)
            . (its.Z.plan.field @"disposeOnUpdated" %~ (<> fin'))
            . (its.Z.model.field @"displayList" .~ []) -- this tells render to update displayItems
    Z.dirty this

-- | Sort the items on the listing given a sorting function
hdlListingSort :: (Z.MonadReactor m)
    => Z.SceneHandler m v (Mapping flt srt k s)
        srt (Which '[])
hdlListingSort this@(Z.Obj ref its) f = Z.terminate' . lift $ do
    Z.doModifyIORef' ref $ \obj ->
        obj & (its.Z.model.field @"displaySort" .~ f)
            . (its.Z.model.field @"displayList" .~ []) -- this tells render to update displayItems
    Z.dirty this

-- | Filter the items on the listing given a filter function
hdlListingFilter :: (Z.MonadReactor m)
    => Z.SceneHandler m v (Mapping flt srt k s)
        flt (Which '[])
hdlListingFilter this@(Z.Obj ref its) f = Z.terminate' . lift $ do
    Z.doModifyIORef' ref $ \obj ->
        obj & (its.Z.model.field @"displayFilter" .~ f)
            . (its.Z.model.field @"displayList" .~ []) -- this tells render to update displayItems
    Z.dirty this

hdlListingInsertItem :: (Z.MonadReactor m, Ord k)
    => Z.Finalizer m s
    -> Z.SceneHandler m v (Mapping flt srt k s)
        (k, s) (Which '[])
hdlListingInsertItem fin this@(Z.Obj ref its) (k, s) = Z.terminate' . lift $ do
    Z.doModifyIORefM ref $ \obj -> do
        let mi = M.lookup k (obj ^. its.Z.model.field @"items")
        fin' <- maybe (pure mempty) fin mi
        pure $ obj & (its.Z.model.field @"items" %~ M.insert k s)
            . (its.Z.plan.field @"disposeOnUpdated" %~ (<> fin'))
            . (its.Z.model.field @"displayList" .~ []) -- this tells render to update displayItems
    Z.dirty this

-- | lift a handler for a single widget into a handler of a list of widgets
-- where the input is broadcast to all the items in the list.
broadcastListingHandler :: (Z.MonadReactor m)
    => Z.Handler m s a b
    -> Z.SceneHandler m v (Mapping flt srt k s)
        a b
broadcastListingHandler hdl (Z.Obj ref its) a = ContT $ \k -> do
    obj <- Z.doReadIORef ref
    traverse_ (\s -> runContT (hdl s a) k) (obj ^. its.Z.model.field @"items")

listingDisplay :: (Z.MonadReactor m)
    => (flt -> s -> m Bool)
    -> (srt -> s -> s -> m Ordering)
    -> Z.Display m s ()
    -> Z.FrameDisplay m (Mapping flt srt k s) ()
listingDisplay flt srt dis (_, Mapping df ds ys xs) = do
    let toLi s = Z.bh "li" []
            (dis s)
        df' = flt df
        ds' = srt ds
    ys' <- lift $ case ys of
            -- if displayList is empty, then run the filter and sort
            [] -> do
                let zs = snd <$> M.toList xs
                zs' <- LM.filterMP df' zs
                zs'' <- LM.sortByM ds' zs'
                pure zs''
            -- else display as is
            ys' -> pure ys'
    Z.bh "ul" []
        (mconcat $ toLi <$> ys')

listingInitializer :: Z.MonadReactor m
    => Z.Initializer m s b
    -> Z.Scene m v (Mapping flt srt k s)
    -> ContT () m b
listingInitializer ini (Z.Obj ref its) = ContT $ \k -> do
    obj <- Z.doReadIORef ref
    traverse_ (\s -> runContT (ini s) k) (obj ^. its.Z.model.field @"items")
