{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Prototypes.Listing.Internal where

import qualified Control.Disposable as CD
import Control.Lens
import Control.Monad.Trans.Class
import qualified Control.Monad.ListM as LM
import qualified Data.DList as DL
import Data.Diverse.Profunctor
import Data.Foldable
import Data.Generics.Product
import Data.IORef
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Semigroup
import qualified Data.Map.Strict as M
import qualified GHC.Generics as G
import qualified Glazier.React as R
import qualified Glazier.React.Framework as F
import qualified Glazier.React.Commands as C
import qualified JavaScript.Extras as JE
import qualified Parameterized.Data.Monoid as P

-- | Internal: Make a key that will fit in between the two provided keys,
-- Except when the inputs are equal, then it will return the same key.
betweenIdx' :: [Int] -> [Int] -> [Int]
betweenIdx' [] [] = []
betweenIdx' [] (y : _) = [y - 1]
betweenIdx' (x : _) [] = [x - 1]
betweenIdx' (x : xs) (y : ys) =
    case compare x y of
        LT -> x : (largerIdx' xs)
        GT -> y : (largerIdx' ys)
        EQ -> x : (betweenIdx' xs ys)

-- | Make a key that will fit in between the two provided keys,
-- Except when the inputs are equal, then it will return the same key.
betweenIdx :: NE.NonEmpty Int -> NE.NonEmpty Int -> NE.NonEmpty Int
betweenIdx (x NE.:| xs) (y NE.:| ys) =
    case compare x y of
        LT -> x NE.:| (largerIdx' xs)
        GT -> y NE.:| (largerIdx' ys)
        EQ -> x NE.:| (betweenIdx' xs ys)

-- | Internal: Create a key larger than the input key.
-- NB. It does not create the smallest key that is larger.
largerIdx' :: [Int] -> [Int]
largerIdx' [] = [0]
largerIdx' (a : _) = [a + 1]

-- | Create a key larger than the input key.
-- NB. It does not create the smallest key that is larger.
largerIdx :: NE.NonEmpty Int -> NE.NonEmpty Int
largerIdx (a NE.:| _) = (a + 1) NE.:| []

-- | Create a key smaller than the input key.
-- NB. It does not create the largest key that is smaller.
smallerIdx :: NE.NonEmpty Int -> NE.NonEmpty Int
smallerIdx (a NE.:| _) = (a - 1) NE.:| []

-- | A listing is actually a map so that we allow for fast insert/deleting
-- and also be able to reorder the elements.
-- Assumption: Once a key is assigned to an item, it is never changed, so the item can use it's key in a callback.
data Listing m a s = Listing
    { filter :: a -> m Bool
    , sort :: a -> a -> m Ordering
    , list :: [a] -- filtered and sorted
    , items :: M.Map (NE.NonEmpty Int) s
    } deriving G.Generic

-- | List specific actions
data ListingDeleteItem = ListingDeleteItem (NE.NonEmpty Int)
data ListingInsertItem s = ListingInsertItem (NE.NonEmpty Int) s
data ListingConsItem s = ListingConsItem s
data ListingSnocItem s = ListingSnocItem s
data ListingMakeItem i f = ListingMakeItem i f
data ListingSort m s = ListingSort (s -> s -> m Ordering)
data ListingFilter m s = ListingFilter (s -> m Bool)

newtype ListingNewItemAction s = ListingNewItemAction {
    runListingNewItemAction :: Which '[ ListingInsertItem s
                                      , ListingConsItem s
                                      , ListingSnocItem s
                                      ] }

newtype ListingAction m i s = ListingAction {
    runListingAction :: Which '[ ListingNewItemAction s
                               , ListingMakeItem i (s -> ListingNewItemAction s)
                               , ListingDeleteItem
                               , ListingSort m s
                               , ListingFilter m s
                               ] }

-- | This version drops the original item handlers @a -> b@, and only have list handlers.
listing :: forall m v i s is ss x c a b.
    ( R.MonadReactor x m
    , HasItem' (Listing m s i) is
    , HasItem' (Listing m s s) ss
    )
  => (s -> [JE.Property]) -> F.Archetype m i s x c a b -> F.Prototype m v is ss
    (Many '[Listing m s i])
    (Many '[Listing m s s])
    x
    c
    (Which '[ListingAction m i s])
    (Which '[C.Rerender])
listing f (F.Archetype (disp, bld@(F.Builder (_, mkMdl)), F.Executor exec, fin)) = F.Prototype
    ( listingDisplay f disp
    , F.toItemBuilder (listingBuilder bld)
    , F.Executor $ \k ->
            let (act, _) = exec k
            in ( F.viaModel (alongside id item') (listingActivator act)
               , F.viaModel (alongside id item') (faceted' (listingRefHandler mkMdl act fin))
               )
    , \ss -> fold <$> (traverse fin (ss ^. item' @(Listing m s s).field @"items"))
    )

-- | Creates a listing with a handler that handles listing actions,
-- as well as broadcasting original actions to in each item in the listing.
broadcastListing :: forall m v i s is ss x c as a3 bs b3.
    ( R.MonadReactor x m
    , HasItem' (Listing m s i) is
    , HasItem' (Listing m s s) ss
    , ChooseBetween '[ListingAction m i s] as a3 '[C.Rerender] bs b3
    )
  => (s -> [JE.Property]) -> F.Archetype m i s x c (Which as) (Which bs) -> F.Prototype m v is ss
    (Many '[Listing m s i])
    (Many '[Listing m s s])
    x
    c
    (Which a3)
    (Which b3)
broadcastListing f (F.Archetype (disp, bld@(F.Builder (_, mkMdl)), F.Executor exec, fin)) = F.Prototype
    ( listingDisplay f disp
    , F.toItemBuilder (listingBuilder bld)
    , F.Executor $ \k ->
            let (act, hdl) = exec k
            in ( F.viaModel (alongside id item') (listingActivator act)
               , F.viaModel (alongside id item') (listingBroadcastRefHandler' mkMdl act hdl fin)
               )
    , \ss -> fold <$> (traverse fin (ss ^. item' @(Listing m s s).field @"items"))
    )

onListingDeleteItem :: (R.MonadReactor x m)
  => (s -> m CD.Disposable)
  -> IORef v
  -> Lens' v (F.ComponentPlan x m, Listing m s s)
  -> ListingDeleteItem
  -> m (DL.DList C.Rerender)
onListingDeleteItem fin ref this (ListingDeleteItem k) = do
       R.doModifyIORefM ref $ \obj -> do
            let mi = M.lookup k (obj ^. this._2.field @"items")
            fin' <- fromMaybe (pure mempty) (fin <$> mi)
            pure $ obj & (this._2.field @"items" %~ M.delete k)
                   . (this._1.field @"disposeOnUpdated" %~ (<> fin'))
                   . (this._2.field @"list" .~ []) -- this tells render to update displayItems
       DL.singleton <$> C.mkRerender ref (this._1) (pure mempty)

-- | Sort the items on the listing given a sorting function
onListingSort :: (R.MonadReactor x m)
  => IORef v
  -> Lens' v (F.ComponentPlan x m, Listing m s s)
  -> ListingSort m s
  -> m (DL.DList C.Rerender)
onListingSort ref this (ListingSort f) = do
       R.doModifyIORef' ref $ \obj ->
           let (Listing df _ _ xs) = obj ^. this._2
           in obj & this._2 .~ (Listing df f [] xs)
       DL.singleton <$> C.mkRerender ref (this._1) (pure mempty)

-- | Filter the items on the listing given a filter function
onListingFilter :: forall x m v s. (R.MonadReactor x m)
  => IORef v
  -> Lens' v (F.ComponentPlan x m, Listing m s s)
  -> ListingFilter m s
  -> m (DL.DList C.Rerender)
onListingFilter ref this (ListingFilter f) = do
       R.doModifyIORef' ref $ \obj ->
           let (Listing _ ds _ xs) = obj ^. this._2
           in obj & this._2 .~ (Listing f ds [] xs)
       DL.singleton <$> C.mkRerender ref (this._1) (pure mempty)

onListingInsertItem :: (R.MonadReactor x m)
  => (s -> m CD.Disposable)
  -> IORef v
  -> Lens' v (F.ComponentPlan x m, Listing m s s)
  -> ListingInsertItem s
  -> m (DL.DList C.Rerender)
onListingInsertItem fin ref this (ListingInsertItem k s) = do
       R.doModifyIORefM ref $ \obj -> do
            let mi = M.lookup k (obj ^. this._2.field @"items")
            fin' <- fromMaybe (pure mempty) (fin <$> mi)
            pure $ obj & (this._2.field @"items" %~ M.insert k s)
                   . (this._1.field @"disposeOnUpdated" %~ (<> fin'))
                   . (this._2.field @"list" .~ []) -- this tells render to update displayItems
       DL.singleton <$> C.mkRerender ref (this._1) (pure mempty)

onListingConsItem :: (R.MonadReactor x m)
  => IORef v
  -> Lens' v (F.ComponentPlan x m, Listing m s s)
  -> ListingConsItem s
  -> m (DL.DList C.Rerender)
onListingConsItem ref this (ListingConsItem s) = do
       R.doModifyIORef' ref $ \obj ->
            let xs = M.toAscList (obj ^. this._2.field @"items")
            in case xs of
                [] -> obj & (this._2.field @"items" .~ M.singleton (0 NE.:| []) s)
                    . (this._2.field @"list" .~ []) -- this tells render to update displayItems
                ((k, _) : _) -> obj & (this._2.field @"items" %~ M.insert (smallerIdx k) s)
                    . (this._2.field @"list" .~ []) -- this tells render to update displayItems
       DL.singleton <$> C.mkRerender ref (this._1) (pure mempty)

onListingSnocItem :: (R.MonadReactor x m)
  => IORef v
  -> Lens' v (F.ComponentPlan x m, Listing m s s)
  -> ListingSnocItem s
  -> m (DL.DList C.Rerender)
onListingSnocItem ref this (ListingSnocItem s) = do
       R.doModifyIORef' ref $ \obj ->
            let xs = M.toDescList (obj ^. this._2.field @"items")
            in case xs of
                [] -> obj & (this._2.field @"items" .~ M.singleton (0 NE.:| []) s)
                    . (this._2.field @"list" .~ []) -- this tells render to update displayItems
                ((k, _) : _) -> obj & (this._2.field @"items" %~ M.insert (largerIdx k) s)
                    . (this._2.field @"list" .~ []) -- this tells render to update displayItems
       DL.singleton <$> C.mkRerender ref (this._1) (pure mempty)

-- | Handler for ListingAction
listingNewItemRefHandler ::
    forall x m s v. ( R.MonadReactor x m)
    => (s -> m CD.Disposable) -> F.RefHandler m v (F.ComponentPlan x m, Listing m s s) (ListingNewItemAction s) C.Rerender
listingNewItemRefHandler fin = F.Handler $ \(ref, Lens this) (ListingNewItemAction a) ->
    switch a . cases $
        (onListingInsertItem @x @m fin ref this)
     ./ (onListingConsItem @x @m ref this)
     ./ (onListingSnocItem @x @m ref this)
     ./ nil

onListingMakeItem :: forall x m i s v. (R.MonadReactor x m)
  => F.MkModel m i s
  -> F.Activator m s
  -> (s -> m CD.Disposable)
  -> IORef v
  -> Lens' v (F.ComponentPlan x m, Listing m s s)
  -> ListingMakeItem i (s -> ListingNewItemAction s)
  -> m (DL.DList C.Rerender)
onListingMakeItem mkMdl act fin ref this (ListingMakeItem i f) = do
    s <- (F.runMkModel mkMdl) i
    (F.runActivator act) s
    (F.runHandler (listingNewItemRefHandler fin)) (ref, Lens this) (f s)

-- | Handler for ListingAction
listingRefHandler ::
    forall m i s v x. ( R.MonadReactor x m
    )
    => F.MkModel m i s
    -> F.Activator m s
    -> (s -> m CD.Disposable)
    -> F.RefHandler m v (F.ComponentPlan x m, Listing m s s) (ListingAction m i s) C.Rerender
listingRefHandler mkMdl act fin = F.Handler $ \v@(ref, Lens this) (ListingAction a) ->
    switch a . cases $
        ((F.runHandler (listingNewItemRefHandler @x @m @s fin)) v)
     ./ (onListingMakeItem @x @m @i @s mkMdl act fin ref this)
     ./ (onListingDeleteItem @x @m fin ref this)
     ./ (onListingSort @x @m ref this)
     ./ (onListingFilter @x @m ref this)
     ./ nil

-- | lift a handler for a single widget into a handler of a list of widgets
-- where the input is broadcast to all the items in the list and the results DList'ed together.
listingBroadcastRefHandler
    ::
    ( R.MonadReactor x m
    )
    => F.Handler m s a b
    -> F.RefHandler m v (F.ComponentPlan x m, Listing m s s) a b
listingBroadcastRefHandler (F.Handler hdl) = F.Handler $ \(ref, Lens this) a -> do
    obj <- R.doReadIORef ref
    ys <- traverse (\x -> hdl x a) (obj ^. this._2.field @"items")
    pure $ fold ys

listingBroadcastRefHandler' ::
    ( R.MonadReactor x m
    , ChooseBetween '[ListingAction m i s] a2 a3 '[C.Rerender] b2 b3
    )
    => F.MkModel m i s
    -> F.Activator m s
    -> F.Handler m s (Which a2) (Which b2)
    -> (s -> m CD.Disposable)
    -> F.RefHandler m v (F.ComponentPlan x m, Listing m s s) (Which a3) (Which b3)

listingBroadcastRefHandler' mkMdl act hdl fin =
    (faceted' (listingRefHandler mkMdl act fin)) `P.pmappend` (listingBroadcastRefHandler hdl)

-- -- | Converts a builder with a plan of @[a]@ to a plan of @Listing a@
-- toListingBuilder

listingBuilder
    :: (Applicative m)
    => F.Builder m i s i s
    -> F.Builder m (Listing m s i) (Listing m s s) (Listing m s i) (Listing m s s)
listingBuilder (F.Builder (F.MkInfo mkInf, F.MkModel mkMdl)) =
    F.Builder (F.MkInfo mkInf', F.MkModel mkMdl')
  where
    mkInf' (Listing df ds di ss) = Listing df ds di <$> (traverse mkInf ss)
    mkMdl' (Listing df ds di ps) = Listing df ds di <$> (traverse mkMdl ps)

listingDisplay
    :: forall m x s ss.
    ( R.MonadReactor x m
    , HasItem' (Listing m s s) ss
    )
    => (s -> [JE.Property])
    -> F.Display m s ()
    -> F.Display m (F.ComponentPlan x m, ss) ()
listingDisplay f (F.Display disp) = F.Display $ \(_, ss) -> do
    let Listing df ds ys xs = ss ^. item' @(Listing m s s)
        toLi s = R.bh "li"
                 []
                 (f s)
                 (disp s)
    ys' <- lift $ case ys of
              [] -> do
                  let zs = snd <$> M.toList xs
                  zs' <- LM.filterMP df zs
                  zs'' <- LM.sortByM ds zs'
                  pure zs''
              ys' -> pure ys'
    R.bh "ul" [] []
        (mconcat $ toLi <$> ys')

listingActivator
    :: R.MonadReactor x m
    => F.Activator m s
    -> F.RefActivator m v (F.ComponentPlan x m, Listing m s s)
listingActivator (F.Activator act) = F.Activator $ \(ref, Lens this) -> do
    obj <- R.doReadIORef ref
    traverse_ (\s -> act s) (obj ^. this._2.field @"items")
