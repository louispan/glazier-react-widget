{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Prototypes.Listing.Internal where

import Control.DeepSeq
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
newtype ListingDeleteItem = ListingDeleteItem (NE.NonEmpty Int)
    deriving (G.Generic, NFData)
data ListingInsertItem s = ListingInsertItem (NE.NonEmpty Int) s
    deriving (G.Generic, NFData)
newtype ListingConsItem s = ListingConsItem s
    deriving (G.Generic, NFData)
newtype ListingSnocItem s = ListingSnocItem s
    deriving (G.Generic, NFData)
data ListingMakeItem i f = ListingMakeItem i f
    deriving (G.Generic, NFData)
newtype ListingSort m s = ListingSort (s -> s -> m Ordering)
    deriving (G.Generic, NFData)
newtype ListingFilter m s = ListingFilter (s -> m Bool)
    deriving (G.Generic, NFData)

newtype ListingNewItemAction s = ListingNewItemAction {
    runListingNewItemAction :: Which '[ ListingInsertItem s
                                      , ListingConsItem s
                                      , ListingSnocItem s
                                      ] }
    deriving (G.Generic, NFData)

newtype ListingAction m i s = ListingAction {
    runListingAction :: Which '[ ListingNewItemAction s
                               , ListingMakeItem i (s -> ListingNewItemAction s)
                               , ListingDeleteItem
                               , ListingSort m s
                               , ListingFilter m s
                               ] }
    deriving (G.Generic, NFData)

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
listing f (F.Archetype (d@(F.Disposer dis), win, bld@(F.Builder (_, mkMdl)), F.Executor exec)) = F.Prototype
    ( F.Disposer $ \ss -> fold <$> (traverse dis (ss ^. item' @(Listing m s s).field @"items"))
    , listingWindow f win
    , F.toItemBuilder (listingBuilder bld)
    , F.Executor $ \k ->
            let (act, _) = exec k
            in ( F.viaModel (alongside id item') (listingActivator act)
               , F.viaModel (alongside id item') (faceted' (listingRefHandler d mkMdl act))
               )
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
broadcastListing f (F.Archetype (d@(F.Disposer dis), win, bld@(F.Builder (_, mkMdl)), F.Executor exec)) = F.Prototype
    ( F.Disposer $ \ss -> fold <$> (traverse dis (ss ^. item' @(Listing m s s).field @"items"))
    , listingWindow f win
    , F.toItemBuilder (listingBuilder bld)
    , F.Executor $ \k ->
            let (act, hdl) = exec k
            in ( F.viaModel (alongside id item') (listingActivator act)
               , F.viaModel (alongside id item') (listingBroadcastRefHandler' d mkMdl act hdl)
               )
    )

whenListingDeleteItem :: (R.MonadReactor x m)
  => F.Disposer m s
  -> IORef v
  -> Lens' v (F.ComponentPlan x m, Listing m s s)
  -> ListingDeleteItem
  -> m (DL.DList C.Rerender)
whenListingDeleteItem (F.Disposer dis) ref this (ListingDeleteItem k) = do
       R.doModifyIORefM ref $ \obj -> do
            let mi = M.lookup k (obj ^. this._2.field @"items")
            dis' <- fromMaybe (pure mempty) (dis <$> mi)
            pure $ obj & (this._2.field @"items" %~ M.delete k)
                   . (this._1.field @"disposeOnUpdated" %~ (<> dis'))
                   . (this._2.field @"list" .~ []) -- this tells render to update displayItems
       DL.singleton <$> C.mkRerender ref (this._1) (pure mempty)

-- | Sort the items on the listing given a sorting function
whenListingSort :: (R.MonadReactor x m)
  => IORef v
  -> Lens' v (F.ComponentPlan x m, Listing m s s)
  -> ListingSort m s
  -> m (DL.DList C.Rerender)
whenListingSort ref this (ListingSort f) = do
       R.doModifyIORef' ref $ \obj ->
           let (Listing df _ _ xs) = obj ^. this._2
           in obj & this._2 .~ (Listing df f [] xs)
       DL.singleton <$> C.mkRerender ref (this._1) (pure mempty)

-- | Filter the items on the listing given a filter function
whenListingFilter :: forall x m v s. (R.MonadReactor x m)
  => IORef v
  -> Lens' v (F.ComponentPlan x m, Listing m s s)
  -> ListingFilter m s
  -> m (DL.DList C.Rerender)
whenListingFilter ref this (ListingFilter f) = do
       R.doModifyIORef' ref $ \obj ->
           let (Listing _ ds _ xs) = obj ^. this._2
           in obj & this._2 .~ (Listing f ds [] xs)
       DL.singleton <$> C.mkRerender ref (this._1) (pure mempty)

whenListingInsertItem :: (R.MonadReactor x m)
  => F.Disposer m s
  -> IORef v
  -> Lens' v (F.ComponentPlan x m, Listing m s s)
  -> ListingInsertItem s
  -> m (DL.DList C.Rerender)
whenListingInsertItem (F.Disposer dis) ref this (ListingInsertItem k s) = do
       R.doModifyIORefM ref $ \obj -> do
            let mi = M.lookup k (obj ^. this._2.field @"items")
            dis' <- fromMaybe (pure mempty) (dis <$> mi)
            pure $ obj & (this._2.field @"items" %~ M.insert k s)
                   . (this._1.field @"disposeOnUpdated" %~ (<> dis'))
                   . (this._2.field @"list" .~ []) -- this tells render to update displayItems
       DL.singleton <$> C.mkRerender ref (this._1) (pure mempty)

whenListingConsItem :: (R.MonadReactor x m)
  => IORef v
  -> Lens' v (F.ComponentPlan x m, Listing m s s)
  -> ListingConsItem s
  -> m (DL.DList C.Rerender)
whenListingConsItem ref this (ListingConsItem s) = do
       R.doModifyIORef' ref $ \obj ->
            let xs = M.toAscList (obj ^. this._2.field @"items")
            in case xs of
                [] -> obj & (this._2.field @"items" .~ M.singleton (0 NE.:| []) s)
                    . (this._2.field @"list" .~ []) -- this tells render to update displayItems
                ((k, _) : _) -> obj & (this._2.field @"items" %~ M.insert (smallerIdx k) s)
                    . (this._2.field @"list" .~ []) -- this tells render to update displayItems
       DL.singleton <$> C.mkRerender ref (this._1) (pure mempty)

whenListingSnocItem :: (R.MonadReactor x m)
  => IORef v
  -> Lens' v (F.ComponentPlan x m, Listing m s s)
  -> ListingSnocItem s
  -> m (DL.DList C.Rerender)
whenListingSnocItem ref this (ListingSnocItem s) = do
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
    => F.Disposer m s -> F.RefHandler m v (F.ComponentPlan x m, Listing m s s) (ListingNewItemAction s) C.Rerender
listingNewItemRefHandler dis = F.Handler $ \(ref, Lens this) (ListingNewItemAction a) ->
    switch a . cases $
        (whenListingInsertItem @x @m dis ref this)
     ./ (whenListingConsItem @x @m ref this)
     ./ (whenListingSnocItem @x @m ref this)
     ./ nil

whenListingMakeItem :: forall x m i s v. (R.MonadReactor x m)
  => F.Disposer m s
  -> F.MkModel m i s
  -> F.Activator m s
  -> IORef v
  -> Lens' v (F.ComponentPlan x m, Listing m s s)
  -> ListingMakeItem i (s -> ListingNewItemAction s)
  -> m (DL.DList C.Rerender)
whenListingMakeItem dis mkMdl act ref this (ListingMakeItem i f) = do
    s <- (F.runMkModel mkMdl) i
    (F.runActivator act) s
    (F.runHandler (listingNewItemRefHandler dis)) (ref, Lens this) (f s)

-- | Handler for ListingAction
listingRefHandler ::
    forall m i s v x. ( R.MonadReactor x m
    )
    => F.Disposer m s
    -> F.MkModel m i s
    -> F.Activator m s
    -> F.RefHandler m v (F.ComponentPlan x m, Listing m s s) (ListingAction m i s) C.Rerender
listingRefHandler dis mkMdl act = F.Handler $ \v@(ref, Lens this) (ListingAction a) ->
    switch a . cases $
        ((F.runHandler (listingNewItemRefHandler @x @m @s dis)) v)
     ./ (whenListingMakeItem @x @m @i @s dis mkMdl act ref this)
     ./ (whenListingDeleteItem @x @m dis ref this)
     ./ (whenListingSort @x @m ref this)
     ./ (whenListingFilter @x @m ref this)
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
    => F.Disposer m s
    -> F.MkModel m i s
    -> F.Activator m s
    -> F.Handler m s (Which a2) (Which b2)
    -> F.RefHandler m v (F.ComponentPlan x m, Listing m s s) (Which a3) (Which b3)

listingBroadcastRefHandler' dis mkMdl act hdl =
    (faceted' (listingRefHandler dis mkMdl act)) `P.pmappend` (listingBroadcastRefHandler hdl)

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

listingWindow
    :: forall m x s ss.
    ( R.MonadReactor x m
    , HasItem' (Listing m s s) ss
    )
    => (s -> [JE.Property])
    -> F.Window m s ()
    -> F.Window m (F.ComponentPlan x m, ss) ()
listingWindow f (F.Window disp) = F.Window $ \(_, ss) -> do
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
