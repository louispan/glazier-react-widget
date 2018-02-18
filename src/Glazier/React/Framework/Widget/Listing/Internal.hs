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
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Framework.Widget.Listing.Internal where

import Control.DeepSeq
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

import qualified Glazier.React.Framework.Core as F
import qualified Glazier.React.Framework.Effect as F
import qualified JavaScript.Extras as JE

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
listing :: forall x m v i s is ss c a b flt srt.
    ( R.MonadReactor x m
    , HasItem' (Listing i flt srt) is
    , HasItem' (Listing s flt srt) ss
    )
    => (flt -> s -> m Bool)
    -> (srt -> s -> s -> m Ordering)
    -> (s -> [JE.Property])
    -> F.Archetype m i s c a b
    -> F.Prototype m v is ss
        (Many '[Listing i flt srt])
        (Many '[Listing s flt srt])
        c
        (Which '[ListingAction flt srt i s])
        c
listing flt srt f (F.Archetype
    (bld@(F.Builder (_, mkSpc)))
        dis
        fin
        act
        _)
    = F.Prototype
    (F.toItemBuilder (listingBuilder bld))
    (listingDisplay flt srt f dis)
    (\s -> fold <$> traverse fin (s ^. item' @(Listing s flt srt).field @"items"))
    (F.viaObj (alongside id (item' @(Listing s flt srt))) (listingActivator act))
    (F.viaObj (alongside id (item' @(Listing s flt srt))) (listingHandler fin mkSpc act))

-- | Creates a listing with a handler that handles listing actions,
-- as well as broadcasting original actions to in each item in the listing.
broadcastListing :: forall x m v i s is ss cs as a3 bs b3 flt srt.
    ( R.MonadReactor x m
    , HasItem' (Listing i flt srt) is
    , HasItem' (Listing s flt srt) ss
    , ChooseBetween '[ListingAction flt srt i s] as a3 cs bs b3
    )
    => (flt -> s -> m Bool)
    -> (srt -> s -> s -> m Ordering)
    -> (s -> [JE.Property])
    -> F.Archetype m i s (Which cs) (Which as) (Which bs)
    -> F.Prototype m v is ss
        (Many '[Listing i flt srt])
        (Many '[Listing s flt srt])
        (Which cs)
        (Which a3)
        (Which b3)
broadcastListing flt srt f arch =
    let F.Prototype bld' dis' fin' act' hdl' = listing flt srt f arch
        hdl = F.handler arch
    in F.Prototype
        bld'
        dis'
        fin'
        act'
        (hdl' `F.orHandler` (F.viaObj (alongside id (item' @(Listing s flt srt))) (broadcastListingHandler hdl)))

hdlListingDeleteItem :: forall x m v s flt srt.
    (R.MonadReactor x m)
    => F.Finalizer m s
    -> F.SceneHandler m v (Listing s flt srt)
        ListingDeleteItem ()
hdlListingDeleteItem fin this@(F.Obj ref its) (ListingDeleteItem k) = lift $ do
    R.doModifyIORefM ref $ \obj -> do
        let mi = M.lookup k (obj ^. its.F.model.field @"items")
        fin' <- maybe (pure mempty) fin mi
        pure $ obj & (its.F.model.field @"items" %~ M.delete k)
            . (its.F.plan.field @"disposeOnUpdated" %~ (<> fin'))
            . (its.F.model.field @"displayList" .~ []) -- this tells render to update displayItems
    F.rerender' this

-- | Sort the items on the listing given a sorting function
hdlListingSort :: (R.MonadReactor x m)
    => F.SceneHandler m v (Listing s flt srt)
    (ListingSort srt) ()
hdlListingSort this@(F.Obj ref its) (ListingSort f) = lift $ do
    R.doModifyIORef' ref $ \obj ->
        obj & (its.F.model.field @"displaySort" .~ f)
            . (its.F.model.field @"displayList" .~ []) -- this tells render to update displayItems
    F.rerender' this

-- | Filter the items on the listing given a filter function
hdlListingFilter :: forall x m v s flt srt.
    (R.MonadReactor x m)
    => F.SceneHandler m v (Listing s flt srt)
        (ListingFilter flt) ()
hdlListingFilter this@(F.Obj ref its) (ListingFilter f) = lift $ do
    R.doModifyIORef' ref $ \obj ->
        obj & (its.F.model.field @"displayFilter" .~ f)
            . (its.F.model.field @"displayList" .~ []) -- this tells render to update displayItems
    F.rerender' this

hdlListingInsertItem :: (R.MonadReactor x m)
    => F.Finalizer m s
    -> F.SceneHandler m v (Listing s flt srt)
        (ListingInsertItem s) ()
hdlListingInsertItem fin this@(F.Obj ref its) (ListingInsertItem k s) = lift $ do
    R.doModifyIORefM ref $ \obj -> do
        let mi = M.lookup k (obj ^. its.F.model.field @"items")
        fin' <- maybe (pure mempty) fin mi
        pure $ obj & (its.F.model.field @"items" %~ M.insert k s)
            . (its.F.plan.field @"disposeOnUpdated" %~ (<> fin'))
            . (its.F.model.field @"displayList" .~ []) -- this tells render to update displayItems
    F.rerender' this

hdlListingConsItem :: (R.MonadReactor x m)
    => F.SceneHandler m v (Listing s flt srt)
        (ListingConsItem s) ()
hdlListingConsItem this@(F.Obj ref its) (ListingConsItem s) = lift $ do
    R.doModifyIORef' ref $ \obj ->
        let xs = M.toAscList (obj ^. its.F.model.field @"items")
        in case xs of
            [] -> obj & (its.F.model.field @"items" .~ M.singleton (0 NE.:| []) s)
                . (its.F.model.field @"displayList" .~ []) -- this tells render to update displayItems
            ((k, _) : _) -> obj & (its.F.model.field @"items" %~ M.insert (smallerIdx k) s)
                . (its.F.model.field @"displayList" .~ []) -- this tells render to update displayItems
    F.rerender' this

hdlListingSnocItem :: (R.MonadReactor x m)
    => F.SceneHandler m v (Listing s flt srt)
        (ListingSnocItem s) ()
hdlListingSnocItem this@(F.Obj ref its) (ListingSnocItem s) = lift $ do
    R.doModifyIORef' ref $ \obj ->
        let xs = M.toDescList (obj ^. its.F.model.field @"items")
        in case xs of
            [] -> obj & (its.F.model.field @"items" .~ M.singleton (0 NE.:| []) s)
                . (its.F.model.field @"displayList" .~ []) -- this tells render to update displayItems
            ((k, _) : _) -> obj & (its.F.model.field @"items" %~ M.insert (largerIdx k) s)
                . (its.F.model.field @"displayList" .~ []) -- this tells render to update displayItems
    F.rerender' this

-- | Handler for ListingAction
hdlListingNewItem ::
    forall x m v s flt srt. (R.MonadReactor x m)
    => F.Finalizer m s
    -> F.SceneHandler m v (Listing s flt srt)
        (ListingNewItemAction s) ()
hdlListingNewItem fin obj (ListingNewItemAction a) =
    switch a . cases $
        hdlListingInsertItem @x @m fin obj
     ./ hdlListingConsItem @x @m obj
     ./ hdlListingSnocItem @x @m obj
     ./ nil

hdlListingMakeItem :: forall x m v i s c flt srt.
    (R.MonadReactor x m)
    => F.Finalizer m s
    -> F.MkSpec m i s
    -> F.Activator m s c
    -> F.SceneHandler m v (Listing s flt srt)
        (ListingMakeItem i (s -> ListingNewItemAction s)) c
hdlListingMakeItem fin mkSpc act obj (ListingMakeItem i f) = do
    s <- lift $ F.runMkSpec mkSpc i
    c <- act s
    hdlListingNewItem fin obj (f s)
    pure c

-- | Handler for ListingAction
listingHandler ::
    forall x m v i s c flt srt. (R.MonadReactor x m)
    => F.Finalizer m s
    -> F.MkSpec m i s
    -> F.Activator m s c
    -> F.SceneHandler m v (Listing s flt srt)
        (Which '[ListingAction flt srt i s]) c
listingHandler fin mkSpc act obj a =
    let ListingAction a' = obvious a
    in switch a' . cases $
        (F.terminate @c . hdlListingNewItem @x @m @_ @s fin obj)
     ./ (hdlListingMakeItem @x @m @_ @i @s fin mkSpc act obj)
     ./ (F.terminate @c . hdlListingDeleteItem @x @m fin obj)
     ./ (F.terminate @c . hdlListingSort @x @m obj)
     ./ (F.terminate @c . hdlListingFilter @x @m obj)
     ./ nil

-- | lift a handler for a single widget into a handler of a list of widgets
-- where the input is broadcast to all the items in the list.
broadcastListingHandler ::
    ( R.MonadReactor x m)
    => F.Handler m s a b
    -> F.SceneHandler m v (Listing s flt srt)
        a b
broadcastListingHandler hdl (F.Obj ref its) a = ContT $ \k -> do
    obj <- R.doReadIORef ref
    traverse_ (\s -> runContT (hdl s a) k) (obj ^. its.F.model.field @"items")

broadcastListingHandler' ::
    ( R.MonadReactor x m
    , ChooseBetween '[ListingAction flt srt i s] a2 a3 c1 b2 c3
    )
    => F.Finalizer m s
    -> F.MkSpec m i s
    -> F.Activator m s (Which c1)
    -> F.Handler m s (Which a2) (Which b2)
    -> F.SceneHandler m v (Listing s flt srt) (Which a3) (Which c3)
broadcastListingHandler' fin mkSpc act hdl =
    (listingHandler fin mkSpc act) `F.orHandler` broadcastListingHandler hdl

-- | Converts a builder with a plan of @[a]@ to a plan of @Listing a@
listingBuilder ::
    (Applicative m)
    => F.Builder m i s i s
    -> F.Builder m (Listing i flt srt) (Listing s flt srt) (Listing i flt srt) (Listing s flt srt)
listingBuilder (F.Builder (F.MkInfo mkInf, F.MkSpec mkSpc)) =
    F.Builder (F.MkInfo mkInf', F.MkSpec mkSpc')
  where
    mkInf' (Listing df ds dss ss) = Listing df ds <$> traverse mkInf dss <*> traverse mkInf ss
    mkSpc' (Listing df ds dps ps) = Listing df ds <$> traverse mkSpc dps <*> traverse mkSpc ps

listingDisplay :: forall flt srt x m s ss.
    ( R.MonadReactor x m
    , HasItem' (Listing s flt srt) ss
    )
    => (flt -> s -> m Bool)
    -> (srt -> s -> s -> m Ordering)
    -> (s -> [JE.Property])
    -> F.Display m s ()
    -> F.FrameDisplay m ss ()
listingDisplay flt srt f dis (_, ss) = do
    let Listing df ds ys xs = ss ^. item' @(Listing s flt srt)
        toLi s = R.branch "li"
            []
            (f s)
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
    R.branch "ul" [] []
        (mconcat $ toLi <$> ys')

listingActivator ::
    R.MonadReactor x m
    => F.Activator m s b
    -> F.Scene m v (Listing s flt srt)
    -> ContT () m b
listingActivator act (F.Obj ref its) = ContT $ \k -> do
    obj <- R.doReadIORef ref
    traverse_ (\s -> runContT (act s) k) (obj ^. its.F.model.field @"items")
