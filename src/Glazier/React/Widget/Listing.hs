{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Widget.Listing (
    -- * Listing index
    betweenIdx
    , largerIdx
    , smallerIdx
    -- * Listing
    , Listing(..)
    , listing
    , listingMakeItem
    , hdlListingDeleteItem
    , hdlListingSort
    , hdlListingFilter
    , hdlListingInsertItem
    , hdlListingConsItem
    , hdlListingSnocItem
    , broadcastListingHandler
    ) where

import Control.Lens
import qualified Control.Monad.ListM as LM
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Data.Diverse.Profunctor
import Data.Foldable
import Data.Generics.Product
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Semigroup
import qualified GHC.Generics as G
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Core as R
import qualified Glazier.React.Framework.Effect as R

-- | Internal: Make a key that will fit in between the two provided keys,
-- Except when the inputs are equal, then it will return the same key.
betweenIdx' :: [Int] -> [Int] -> [Int]
betweenIdx' [] [] = []
betweenIdx' [] (y : _) = [y - 1]
betweenIdx' (x : _) [] = [x - 1]
betweenIdx' (x : xs) (y : ys) =
    case compare x y of
        LT -> x : largerIdx' xs
        GT -> y : largerIdx' ys
        EQ -> x : betweenIdx' xs ys

-- | Make a key that will fit in between the two provided keys,
-- Except when the inputs are equal, then it will return the same key.
betweenIdx :: NE.NonEmpty Int -> NE.NonEmpty Int -> NE.NonEmpty Int
betweenIdx (x NE.:| xs) (y NE.:| ys) =
    case compare x y of
        LT -> x NE.:| largerIdx' xs
        GT -> y NE.:| largerIdx' ys
        EQ -> x NE.:| betweenIdx' xs ys

-- | Internal: Create a key larger than the input key.
-- NB. It does not create the smallest key that is largeR.
largerIdx' :: [Int] -> [Int]
largerIdx' [] = [0]
largerIdx' (a : _) = [a + 1]

-- | Create a key larger than the input key.
-- NB. It does not create the smallest key that is largeR.
largerIdx :: NE.NonEmpty Int -> NE.NonEmpty Int
largerIdx (a NE.:| _) = (a + 1) NE.:| []

-- | Create a key smaller than the input key.
-- NB. It does not create the largest key that is smalleR.
smallerIdx :: NE.NonEmpty Int -> NE.NonEmpty Int
smallerIdx (a NE.:| _) = (a - 1) NE.:| []

-- | A listing is actually a map so that we allow for fast insert/deleting
-- and also be able to reorder the elements.
-- Assumption: Once a key is assigned to an item, it is never changed, so the item can use it's key in a callback.
data Listing flt srt i = Listing
    { displayFilter :: flt
    , displaySort :: srt
    , displayList :: [i] -- filtered and sorted
    , items :: M.Map (NE.NonEmpty Int) i
    } deriving (G.Generic, Functor)


-- | This version drops the original item handlers @a -> b@, and only have list handlers.
listing :: forall m v i s is ss c flt srt.
    (R.MonadReactor m)
    => (is -> (Listing flt srt i))
    -> Lens' ss (Listing flt srt s)
    -> (flt -> s -> m Bool)
    -> (srt -> s -> s -> m Ordering)
    -> R.Archetype m i s c
    -> R.Prototype m v is ss
        (Many '[Listing flt srt i])
        (Many '[Listing flt srt s])
        c
listing fi fs flt srt (R.Archetype
    bld
    dis
    fin
    act)
    =
    let p = R.Prototype
            (bimap single single (listingBuilder bld))
            (listingDisplay flt srt dis)
            R.nulFinalizer
            (listingActivator act)
    in (R.byPrototype fi fs p)
        { R.finalizer = \s -> fold <$> traverse fin (s ^. fs.field @"items") }

hdlListingDeleteItem :: (R.MonadReactor m)
    => R.Finalizer m s
    -> R.SceneHandler m v (Listing flt srt s)
        (NE.NonEmpty Int) (Which '[])
hdlListingDeleteItem fin this@(R.Obj ref its) k = R.terminate' . lift $ do
    R.doModifyIORefM ref $ \obj -> do
        let mi = M.lookup k (obj ^. its.R.model.field @"items")
        fin' <- maybe (pure mempty) fin mi
        pure $ obj & (its.R.model.field @"items" %~ M.delete k)
            . (its.R.plan.field @"disposeOnUpdated" %~ (<> fin'))
            . (its.R.model.field @"displayList" .~ []) -- this tells render to update displayItems
    R.rerender' this

-- | Sort the items on the listing given a sorting function
hdlListingSort :: (R.MonadReactor m)
    => R.SceneHandler m v (Listing flt srt s)
        srt (Which '[])
hdlListingSort this@(R.Obj ref its) f = R.terminate' . lift $ do
    R.doModifyIORef' ref $ \obj ->
        obj & (its.R.model.field @"displaySort" .~ f)
            . (its.R.model.field @"displayList" .~ []) -- this tells render to update displayItems
    R.rerender' this

-- | Filter the items on the listing given a filter function
hdlListingFilter :: (R.MonadReactor m)
    => R.SceneHandler m v (Listing flt srt s)
        flt (Which '[])
hdlListingFilter this@(R.Obj ref its) f = R.terminate' . lift $ do
    R.doModifyIORef' ref $ \obj ->
        obj & (its.R.model.field @"displayFilter" .~ f)
            . (its.R.model.field @"displayList" .~ []) -- this tells render to update displayItems
    R.rerender' this

hdlListingInsertItem :: (R.MonadReactor m)
    => R.Finalizer m s
    -> R.SceneHandler m v (Listing flt srt s)
        (NE.NonEmpty Int, s) (Which '[])
hdlListingInsertItem fin this@(R.Obj ref its) (k, s) = R.terminate' . lift $ do
    R.doModifyIORefM ref $ \obj -> do
        let mi = M.lookup k (obj ^. its.R.model.field @"items")
        fin' <- maybe (pure mempty) fin mi
        pure $ obj & (its.R.model.field @"items" %~ M.insert k s)
            . (its.R.plan.field @"disposeOnUpdated" %~ (<> fin'))
            . (its.R.model.field @"displayList" .~ []) -- this tells render to update displayItems
    R.rerender' this

hdlListingConsItem :: (R.MonadReactor m)
    => R.SceneHandler m v (Listing flt srt s)
        s (Which '[])
hdlListingConsItem this@(R.Obj ref its) s = R.terminate' . lift $ do
    R.doModifyIORef' ref $ \obj ->
        let xs = M.toAscList (obj ^. its.R.model.field @"items")
        in case xs of
            [] -> obj & (its.R.model.field @"items" .~ M.singleton (0 NE.:| []) s)
                . (its.R.model.field @"displayList" .~ []) -- this tells render to update displayItems
            ((k, _) : _) -> obj & (its.R.model.field @"items" %~ M.insert (smallerIdx k) s)
                . (its.R.model.field @"displayList" .~ []) -- this tells render to update displayItems
    R.rerender' this

hdlListingSnocItem :: (R.MonadReactor m)
    => R.SceneHandler m v (Listing flt srt s)
        s (Which '[])
hdlListingSnocItem this@(R.Obj ref its) s = R.terminate' . lift $ do
    R.doModifyIORef' ref $ \obj ->
        let xs = M.toDescList (obj ^. its.R.model.field @"items")
        in case xs of
            [] -> obj & (its.R.model.field @"items" .~ M.singleton (0 NE.:| []) s)
                . (its.R.model.field @"displayList" .~ []) -- this tells render to update displayItems
            ((k, _) : _) -> obj & (its.R.model.field @"items" %~ M.insert (largerIdx k) s)
                . (its.R.model.field @"displayList" .~ []) -- this tells render to update displayItems
    R.rerender' this

listingMakeItem :: (R.MonadReactor m)
    => R.MkSpec m i s
    -> R.Activator m s c
    -> i
    -> ContT () m (c, s)
listingMakeItem mkSpc act i = do
    s <- lift $ R.unMkSpec mkSpc i
    c <- act s
    pure (c, s)

-- | lift a handler for a single widget into a handler of a list of widgets
-- where the input is broadcast to all the items in the list.
broadcastListingHandler :: (R.MonadReactor m)
    => R.Handler m s a b
    -> R.SceneHandler m v (Listing flt srt s)
        a b
broadcastListingHandler hdl (R.Obj ref its) a = ContT $ \k -> do
    obj <- R.doReadIORef ref
    traverse_ (\s -> runContT (hdl s a) k) (obj ^. its.R.model.field @"items")

-- | Converts a builder with a plan of @[a]@ to a plan of @Listing a@
listingBuilder :: (Applicative m)
    => R.Builder m i s i s
    -> R.Builder m (Listing flt srt i) (Listing flt srt s) (Listing flt srt i) (Listing flt srt s)
listingBuilder (R.Builder (R.MkInfo mkInf, R.MkSpec mkSpc)) =
    R.Builder (R.MkInfo mkInf', R.MkSpec mkSpc')
  where
    mkInf' (Listing df ds dss ss) = Listing df ds <$> traverse mkInf dss <*> traverse mkInf ss
    mkSpc' (Listing df ds dps ps) = Listing df ds <$> traverse mkSpc dps <*> traverse mkSpc ps

listingDisplay :: (R.MonadReactor m)
    => (flt -> s -> m Bool)
    -> (srt -> s -> s -> m Ordering)
    -> R.Display m s ()
    -> R.FrameDisplay m (Listing flt srt s) ()
listingDisplay flt srt dis (_, Listing df ds ys xs) = do
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

listingActivator :: R.MonadReactor m
    => R.Activator m s b
    -> R.Scene m v (Listing flt srt s)
    -> ContT () m b
listingActivator act (R.Obj ref its) = ContT $ \k -> do
    obj <- R.doReadIORef ref
    traverse_ (\s -> runContT (act s) k) (obj ^. its.R.model.field @"items")
