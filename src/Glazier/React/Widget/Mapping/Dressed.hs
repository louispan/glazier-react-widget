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
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Core as R

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
    => R.Builder m i s i s
    -> R.Builder m (Mapping flt srt k i) (Mapping flt srt k s) (Mapping flt srt k i) (Mapping flt srt k s)
listingBuilder (R.Builder (R.MkReq mkReq, R.MkSpec mkSpc)) =
    R.Builder (R.MkReq mkReq', R.MkSpec mkSpc')
  where
    mkReq' (Mapping df ds dss ss) = Mapping df ds <$> traverse mkReq dss <*> traverse mkReq ss
    mkSpc' (Mapping df ds dps ps) = Mapping df ds <$> traverse mkSpc dps <*> traverse mkSpc ps

-- | This version drops the original item handlers @a -> b@, and only have list handlers.
listing :: (R.MonadReactor m)
    => (flt -> s -> m Bool)
    -> (srt -> s -> s -> m Ordering)
    -> R.Archetype m s c
    -> R.Prototype m v (Mapping flt srt k s) c
listing flt srt (R.Archetype dis fin ini)
    = R.Prototype
        (listingDisplay flt srt dis)
        (\s -> fold <$> traverse fin (s ^. field @"items"))
        (listingInitializer ini)

hdlListingDeleteItem :: (R.MonadReactor m, Ord k)
    => R.Finalizer m s
    -> R.SceneHandler m v (Mapping flt srt k s)
        k (Which '[])
hdlListingDeleteItem fin this@(R.Obj ref its) k = R.terminate' . lift $ do
    R.doModifyIORefM ref $ \obj -> do
        let mi = M.lookup k (obj ^. its.R.model.field @"items")
        fin' <- maybe (pure mempty) fin mi
        pure $ obj & (its.R.model.field @"items" %~ M.delete k)
            . (its.R.plan.field @"disposeOnUpdated" %~ (<> fin'))
            . (its.R.model.field @"displayList" .~ []) -- this tells render to update displayItems
    R.dirty this

-- | Sort the items on the listing given a sorting function
hdlListingSort :: (R.MonadReactor m)
    => R.SceneHandler m v (Mapping flt srt k s)
        srt (Which '[])
hdlListingSort this@(R.Obj ref its) f = R.terminate' . lift $ do
    R.doModifyIORef' ref $ \obj ->
        obj & (its.R.model.field @"displaySort" .~ f)
            . (its.R.model.field @"displayList" .~ []) -- this tells render to update displayItems
    R.dirty this

-- | Filter the items on the listing given a filter function
hdlListingFilter :: (R.MonadReactor m)
    => R.SceneHandler m v (Mapping flt srt k s)
        flt (Which '[])
hdlListingFilter this@(R.Obj ref its) f = R.terminate' . lift $ do
    R.doModifyIORef' ref $ \obj ->
        obj & (its.R.model.field @"displayFilter" .~ f)
            . (its.R.model.field @"displayList" .~ []) -- this tells render to update displayItems
    R.dirty this

hdlListingInsertItem :: (R.MonadReactor m, Ord k)
    => R.Finalizer m s
    -> R.SceneHandler m v (Mapping flt srt k s)
        (k, s) (Which '[])
hdlListingInsertItem fin this@(R.Obj ref its) (k, s) = R.terminate' . lift $ do
    R.doModifyIORefM ref $ \obj -> do
        let mi = M.lookup k (obj ^. its.R.model.field @"items")
        fin' <- maybe (pure mempty) fin mi
        pure $ obj & (its.R.model.field @"items" %~ M.insert k s)
            . (its.R.plan.field @"disposeOnUpdated" %~ (<> fin'))
            . (its.R.model.field @"displayList" .~ []) -- this tells render to update displayItems
    R.dirty this

listingMakeItem :: (R.MonadReactor m)
    => R.MkSpec m i s
    -> R.Initializer m s c
    -> i
    -> ContT () m (c, s)
listingMakeItem mkSpc ini i = do
    s <- lift $ R.unMkSpec mkSpc i
    c <- ini s
    pure (c, s)

-- | lift a handler for a single widget into a handler of a list of widgets
-- where the input is broadcast to all the items in the list.
broadcastListingHandler :: (R.MonadReactor m)
    => R.Handler m s a b
    -> R.SceneHandler m v (Mapping flt srt k s)
        a b
broadcastListingHandler hdl (R.Obj ref its) a = ContT $ \k -> do
    obj <- R.doReadIORef ref
    traverse_ (\s -> runContT (hdl s a) k) (obj ^. its.R.model.field @"items")

listingDisplay :: (R.MonadReactor m)
    => (flt -> s -> m Bool)
    -> (srt -> s -> s -> m Ordering)
    -> R.Display m s ()
    -> R.FrameDisplay m (Mapping flt srt k s) ()
listingDisplay flt srt dis (_, Mapping df ds ys xs) = do
    let toLi s = R.bh "li" []
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
    R.bh "ul" []
        (mconcat $ toLi <$> ys')

listingInitializer :: R.MonadReactor m
    => R.Initializer m s b
    -> R.Scene m v (Mapping flt srt k s)
    -> ContT () m b
listingInitializer ini (R.Obj ref its) = ContT $ \k -> do
    obj <- R.doReadIORef ref
    traverse_ (\s -> runContT (ini s) k) (obj ^. its.R.model.field @"items")
