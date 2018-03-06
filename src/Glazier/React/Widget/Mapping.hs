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
import Glazier.React
import Glazier.React.Framework.Core

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
    => Builder m i s i s
    -> Builder m (Mapping flt srt k i) (Mapping flt srt k s) (Mapping flt srt k i) (Mapping flt srt k s)
listingBuilder (Builder (MkReq mkReq, MkSpec mkSpc)) =
    Builder (MkReq mkReq', MkSpec mkSpc')
  where
    mkReq' (Mapping df ds dss ss) = Mapping df ds <$> traverse mkReq dss <*> traverse mkReq ss
    mkSpc' (Mapping df ds dps ps) = Mapping df ds <$> traverse mkSpc dps <*> traverse mkSpc ps

-- | This version drops the original item handlers @a -> b@, and only have list handlers.
listing :: (MonadReactor m)
    => (flt -> s -> m Bool)
    -> (srt -> s -> s -> m Ordering)
    -> Archetype s m c
    -> Prototype m p (Mapping flt srt k s) c
listing flt srt (Archetype dis fin ini)
    = Prototype
        (listingDisplay flt srt dis)
        (\s -> fold <$> traverse fin (s ^. field @"items"))
        (listingInitializer ini)

hdlListingDeleteItem :: (MonadReactor m, Ord k)
    => Finalizer s m
    -> SceneHandler m p (Mapping flt srt k s)
        k (Which '[])
hdlListingDeleteItem fin this@(Obj ref its) k = terminate' . lift $ do
    doModifyIORefM ref $ \obj -> do
        let mi = M.lookup k (obj ^. my._model.field @"items")
        fin' <- maybe (pure mempty) fin mi
        pure $ obj & (my._model.field @"items" %~ M.delete k)
            . (my._plan._disposeOnUpdated %~ (<> fin'))
            . (my._model.field @"displayList" .~ []) -- this tells render to update displayItems
    dirty this

-- | Sort the items on the listing given a sorting function
hdlListingSort :: (MonadReactor m)
    => SceneHandler m p (Mapping flt srt k s)
        srt (Which '[])
hdlListingSort this@(Obj ref its) f = terminate' . lift $ do
    doModifyIORef' ref $ \obj ->
        obj & (my._model.field @"displaySort" .~ f)
            . (my._model.field @"displayList" .~ []) -- this tells render to update displayItems
    dirty this

-- | Filter the items on the listing given a filter function
hdlListingFilter :: (MonadReactor m)
    => SceneHandler m p (Mapping flt srt k s)
        flt (Which '[])
hdlListingFilter this@(Obj ref its) f = terminate' . lift $ do
    doModifyIORef' ref $ \obj ->
        obj & (my._model.field @"displayFilter" .~ f)
            . (my._model.field @"displayList" .~ []) -- this tells render to update displayItems
    dirty this

hdlListingInsertItem :: (MonadReactor m, Ord k)
    => Finalizer s m
    -> SceneHandler m p (Mapping flt srt k s)
        (k, s) (Which '[])
hdlListingInsertItem fin this@(Obj ref its) (k, s) = terminate' . lift $ do
    doModifyIORefM ref $ \obj -> do
        let mi = M.lookup k (obj ^. my._model.field @"items")
        fin' <- maybe (pure mempty) fin mi
        pure $ obj & (my._model.field @"items" %~ M.insert k s)
            . (my._plan._disposeOnUpdated %~ (<> fin'))
            . (my._model.field @"displayList" .~ []) -- this tells render to update displayItems
    dirty this

-- | lift a handler for a single widget into a handler of a list of widgets
-- where the input is broadcast to all the items in the list.
broadcastListingHandler :: (MonadReactor m)
    => Handler m s a b
    -> SceneHandler m p (Mapping flt srt k s)
        a b
broadcastListingHandler hdl (Obj ref its) a = ContT $ \k -> do
    me <- doReadIORef this
    traverse_ (\s -> runContT (hdl s a) k) (obj ^. my._model.field @"items")

listingDisplay :: (MonadReactor m)
    => (flt -> s -> m Bool)
    -> (srt -> s -> s -> m Ordering)
    -> Display s m ()
    -> FrameDisplay m (Mapping flt srt k s) ()
listingDisplay flt srt dis (_, Mapping df ds ys xs) = do
    let toLi s = bh "li" []
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
    bh "ul" []
        (mconcat $ toLi <$> ys')

listingInitializer :: MonadReactor m
    => Initializer m s b
    -> Scene m p (Mapping flt srt k s)
    -> ContT () m b
listingInitializer ini (Obj ref its) = ContT $ \k -> do
    me <- doReadIORef this
    traverse_ (\s -> runContT (ini s) k) (obj ^. my._model.field @"items")
