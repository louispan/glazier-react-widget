{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Widget.Prototypes.Listing.Internal where

import Control.DeepSeq
import Control.Lens
import qualified Control.Monad.ListM as LM
import Control.Monad.Trans.Class
import Data.Diverse.Profunctor
import qualified Data.DList as DL
import Data.Foldable
import Data.Generics.Product
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Semigroup
import Data.Void
import qualified GHC.Generics as G
import qualified Glazier.React as R
import qualified Glazier.React.Framework as F
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
data Listing i flt srt = Listing
    { displayFilter :: flt
    , displaySort :: srt
    , displayList :: [i] -- filtered and sorted
    , items :: M.Map (NE.NonEmpty Int) i
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
newtype ListingSort srt = ListingSort srt
    deriving (G.Generic, NFData)
newtype ListingFilter flt = ListingFilter flt
    deriving (G.Generic, NFData)

newtype ListingNewItemAction s = ListingNewItemAction {
    runListingNewItemAction :: Which '[ ListingInsertItem s
                                      , ListingConsItem s
                                      , ListingSnocItem s
                                      ] }
    deriving (G.Generic, NFData)

newtype ListingAction flt srt i s = ListingAction {
    runListingAction :: Which '[ ListingNewItemAction s
                               , ListingMakeItem i (s -> ListingNewItemAction s)
                               , ListingDeleteItem
                               , ListingFilter flt
                               , ListingSort srt
                               ] }
    deriving (G.Generic, NFData)

-- | This version drops the original item handlers @a -> b@, and only have list handlers.
listing :: forall x m v i s is ss y z a b flt srt.
    ( R.MonadReactor x m
    , HasItem' (Listing i flt srt) is
    , HasItem' (Listing s flt srt) ss
    )
    => (flt -> s -> m Bool)
    -> (srt -> s -> s -> m Ordering)
    -> (s -> [JE.Property])
    -> F.Archetype x m i s y z a b
    -> F.Prototype x m v is ss
        (Many '[Listing i flt srt])
        (Many '[Listing s flt srt])
        y
        y
        (Which '[ListingAction flt srt i s])
        (Which '[])
listing flt srt f (F.Archetype
    (bld@(F.Builder (_, mkMdl)))
        dis
        (d@(F.Finalizer fin))
        (F.Executor xact)
        _)
    = F.Prototype
    (F.toItemBuilder (listingBuilder bld))
    (listingDisplay flt srt f dis)
    (F.Finalizer $ \ss -> fold <$> traverse fin (ss ^. item' @(Listing s flt srt).field @"items"))
    (F.Executor $ \k ->
        let act = xact k
        in F.viaModel (alongside id (item' @(Listing s flt srt))) (listingActivator act))
    (F.Executor $ \k ->
        let act = xact k
        in F.viaModel (alongside id item') (rmap (impossible' @(Which '[])) (faceted' (listingHandler d mkMdl act))))

-- | Creates a listing with a handler that handles listing actions,
-- as well as broadcasting original actions to in each item in the listing.
broadcastListing :: forall x m v i s is ss ys zs as a3 bs b3 flt srt.
    ( R.MonadReactor x m
    , HasItem' (Listing i flt srt) is
    , HasItem' (Listing s flt srt) ss
    , ChooseBetween '[ListingAction flt srt i s] as a3 '[] bs b3
    , Diversify ys (AppendUnique ys zs)
    , Diversify zs (AppendUnique ys zs)
    )
    => (flt -> s -> m Bool)
    -> (srt -> s -> s -> m Ordering)
    -> (s -> [JE.Property])
    -> F.Archetype x m i s (Which ys) (Which zs) (Which as) (Which bs)
    -> F.Prototype x m v is ss
        (Many '[Listing i flt srt])
        (Many '[Listing s flt srt])
        (Which ys)
        (Which (AppendUnique ys zs))
        (Which a3)
        (Which b3)
broadcastListing flt srt f (F.Archetype
        (bld@(F.Builder (_, mkMdl)))
        dis
        (d@(F.Finalizer fin))
        xact
        xhdl)
    = F.Prototype
    (F.toItemBuilder (listingBuilder bld))
    (listingDisplay flt srt f dis)
    (F.Finalizer $ \ss -> fold <$> traverse fin (ss ^. item' @(Listing s flt srt).field @"items"))
    (F.Executor $ \k ->
        let act = F.runExecutor xact k
        in F.viaModel (alongside id (item' @(Listing s flt srt))) (listingActivator act))
    (F.Executor $ \k ->
        let act = F.runExecutor (F.withExecutor diversify xact) k
            hdl = F.runExecutor (F.withExecutor diversify xhdl) k
    in F.viaModel (alongside id item') (listingBroadcastHandler' d mkMdl act hdl))

whenListingDeleteItem :: (R.MonadReactor x m)
    => F.Finalizer m s
    -> F.Scene x m v (Listing s flt srt)
    -> ListingDeleteItem
    -> m (DL.DList Void)
whenListingDeleteItem (F.Finalizer fin) v@(F.Obj ref (Lens this)) (ListingDeleteItem k) = do
    R.doModifyIORefM ref $ \obj -> do
        let mi = M.lookup k (obj ^. this._2.field @"items")
        fin' <- maybe (pure mempty) fin mi
        pure $ obj & (this._2.field @"items" %~ M.delete k)
            . (this._1.field @"disposeOnUpdated" %~ (<> fin'))
            . (this._2.field @"displayList" .~ []) -- this tells render to update displayItems
    F.rerender v
    pure mempty

-- | Sort the items on the listing given a sorting function
whenListingSort :: (R.MonadReactor x m)
  => F.Scene x m v (Listing s flt srt)
  -> ListingSort srt
  -> m (DL.DList Void)
whenListingSort v@(F.Obj ref (Lens this)) (ListingSort f) = do
    R.doModifyIORef' ref $ \obj ->
        obj & (this._2.field @"displaySort" .~ f)
            . (this._2.field @"displayList" .~ []) -- this tells render to update displayItems
    F.rerender v
    pure mempty

-- | Filter the items on the listing given a filter function
whenListingFilter :: forall x m v s flt srt.
    (R.MonadReactor x m)
    => F.Scene x m v (Listing s flt srt)
    -> ListingFilter flt
    -> m (DL.DList Void)
whenListingFilter v@(F.Obj ref (Lens this)) (ListingFilter f) = do
    R.doModifyIORef' ref $ \obj ->
        obj & (this._2.field @"displayFilter" .~ f)
            . (this._2.field @"displayList" .~ []) -- this tells render to update displayItems
    F.rerender v
    pure mempty

whenListingInsertItem :: (R.MonadReactor x m)
    => F.Finalizer m s
    -> F.Scene x m v (Listing s flt srt)
    -> ListingInsertItem s
    -> m (DL.DList Void)
whenListingInsertItem (F.Finalizer fin) v@(F.Obj ref (Lens this)) (ListingInsertItem k s) = do
    R.doModifyIORefM ref $ \obj -> do
        let mi = M.lookup k (obj ^. this._2.field @"items")
        fin' <- maybe (pure mempty) fin mi
        pure $ obj & (this._2.field @"items" %~ M.insert k s)
            . (this._1.field @"disposeOnUpdated" %~ (<> fin'))
            . (this._2.field @"displayList" .~ []) -- this tells render to update displayItems
    F.rerender v
    pure mempty

whenListingConsItem :: (R.MonadReactor x m)
    => F.Scene x m v (Listing s flt srt)
    -> ListingConsItem s
    -> m (DL.DList Void)
whenListingConsItem v@(F.Obj ref (Lens this)) (ListingConsItem s) = do
    R.doModifyIORef' ref $ \obj ->
        let xs = M.toAscList (obj ^. this._2.field @"items")
        in case xs of
            [] -> obj & (this._2.field @"items" .~ M.singleton (0 NE.:| []) s)
                . (this._2.field @"displayList" .~ []) -- this tells render to update displayItems
            ((k, _) : _) -> obj & (this._2.field @"items" %~ M.insert (smallerIdx k) s)
                . (this._2.field @"displayList" .~ []) -- this tells render to update displayItems
    F.rerender v
    pure mempty

whenListingSnocItem :: (R.MonadReactor x m)
    => F.Scene x m v (Listing s flt srt)
    -> ListingSnocItem s
    -> m (DL.DList Void)
whenListingSnocItem v@(F.Obj ref (Lens this)) (ListingSnocItem s) = do
    R.doModifyIORef' ref $ \obj ->
        let xs = M.toDescList (obj ^. this._2.field @"items")
        in case xs of
            [] -> obj & (this._2.field @"items" .~ M.singleton (0 NE.:| []) s)
                . (this._2.field @"displayList" .~ []) -- this tells render to update displayItems
            ((k, _) : _) -> obj & (this._2.field @"items" %~ M.insert (largerIdx k) s)
                . (this._2.field @"displayList" .~ []) -- this tells render to update displayItems
    F.rerender v
    pure mempty

-- | Handler for ListingAction
listingNewItemHandler ::
    forall x m v s flt srt. ( R.MonadReactor x m)
    => F.Finalizer m s -> F.SceneHandler x m v (Listing s flt srt) (ListingNewItemAction s) Void
listingNewItemHandler fin = F.Handler $ \obj (ListingNewItemAction a) ->
    switch a . cases $
        whenListingInsertItem @x @m fin obj
     ./ whenListingConsItem @x @m obj
     ./ whenListingSnocItem @x @m obj
     ./ nil

whenListingMakeItem :: forall x m v i s flt srt. (R.MonadReactor x m)
    => F.Finalizer m s
    -> F.MkModel m i s
    -> F.Activator m s
    -> F.Scene x m v (Listing s flt srt)
    -> ListingMakeItem i (s -> ListingNewItemAction s)
    -> m (DL.DList Void)
whenListingMakeItem fin mkMdl act obj (ListingMakeItem i f) = do
    s <- F.runMkModel mkMdl i
    F.runActivator act s
    F.runHandler (listingNewItemHandler fin) obj (f s)

-- | Handler for ListingAction
listingHandler ::
    forall x m v i s flt srt. ( R.MonadReactor x m
    )
    => F.Finalizer m s
    -> F.MkModel m i s
    -> F.Activator m s
    -> F.SceneHandler x m v (Listing s flt srt) (ListingAction flt srt i s) Void
listingHandler fin mkMdl act = F.Handler $ \obj (ListingAction a) ->
    switch a . cases $
        F.runHandler (listingNewItemHandler @x @m @_ @s fin) obj
     ./ whenListingMakeItem @x @m @_ @i @s fin mkMdl act obj
     ./ whenListingDeleteItem @x @m fin obj
     ./ whenListingSort @x @m obj
     ./ whenListingFilter @x @m obj
     ./ nil

-- | lift a handler for a single widget into a handler of a list of widgets
-- where the input is broadcast to all the items in the list and the results DList'ed together.
listingBroadcastHandler ::
    ( R.MonadReactor x m
    )
    => F.Handler m s a b
    -> F.SceneHandler x m v (Listing s flt srt) a b
listingBroadcastHandler (F.Handler hdl) = F.Handler $ \(F.Obj ref (Lens this)) a -> do
    obj <- R.doReadIORef ref
    ys <- traverse (`hdl` a) (obj ^. this._2.field @"items")
    pure $ fold ys

listingBroadcastHandler' ::
    ( R.MonadReactor x m
    , ChooseBetween '[ListingAction flt srt i s] a2 a3 '[] b2 b3
    )
    => F.Finalizer m s
    -> F.MkModel m i s
    -> F.Activator m s
    -> F.Handler m s (Which a2) (Which b2)
    -> F.SceneHandler x m v (Listing s flt srt) (Which a3) (Which b3)
listingBroadcastHandler' fin mkMdl act hdl =
    (rmap (impossible' @(Which '[])) (faceted' (listingHandler fin mkMdl act))) `P.pmappend` listingBroadcastHandler hdl

-- | Converts a builder with a plan of @[a]@ to a plan of @Listing a@
listingBuilder ::
    (Applicative m)
    => F.Builder m i s i s
    -> F.Builder m (Listing i flt srt) (Listing s flt srt) (Listing i flt srt) (Listing s flt srt)
listingBuilder (F.Builder (F.MkInfo mkInf, F.MkModel mkMdl)) =
    F.Builder (F.MkInfo mkInf', F.MkModel mkMdl')
  where
    mkInf' (Listing df ds dss ss) = Listing df ds <$> traverse mkInf dss <*> traverse mkInf ss
    mkMdl' (Listing df ds dps ps) = Listing df ds <$> traverse mkMdl dps <*> traverse mkMdl ps

listingDisplay :: forall flt srt x m s ss.
    ( R.MonadReactor x m
    , HasItem' (Listing s flt srt) ss
    )
    => (flt -> s -> m Bool)
    -> (srt -> s -> s -> m Ordering)
    -> (s -> [JE.Property])
    -> F.Display m s ()
    -> F.PlanDisplay x m ss ()
listingDisplay flt srt f (F.Display disp) = F.Display $ \(_, ss) -> do
    let Listing df ds ys xs = ss ^. item' @(Listing s flt srt)
        toLi s = R.branch "li"
            []
            (f s)
            (disp s)
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
    R.branch "ul" [] []
        (mconcat $ toLi <$> ys')

listingActivator ::
    R.MonadReactor x m
    => F.Activator m s
    -> F.SceneActivator x m v (Listing s flt srt)
listingActivator (F.Activator act) = F.Activator $ \(F.Obj ref (Lens this)) -> do
    obj <- R.doReadIORef ref
    traverse_ act (obj ^. this._2.field @"items")
