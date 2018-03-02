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

module Glazier.React.Widget.Pile.Glam where

import Control.Lens
import qualified Control.Monad.ListM as LM
import Control.Monad.Trans.Class
import Data.Foldable
import Data.Generics.Product
import qualified GHC.Generics as G
import qualified Glazier.React.Framework as Z
import qualified Glazier.React.Widget.Pile as W

-- | Contains information on sorting and filtering the items in a pile
-- differerently from the native data structure.
data GlamPile flt srt t a = GlamPile
    { filterCriteria :: flt
    , sortCriteria :: srt
    , glamList :: [a] -- filtered and sorted
    , rawPile :: t a
    } deriving (G.Generic, Functor)

-- | Converts a builder with a plan of @[a]@ to a plan of @Mapping a@
glamPileBuilder :: (Traversable t, Applicative m)
    => Z.Builder m r s r' s'
    -> Z.Builder m (GlamPile flt srt t r) (GlamPile flt srt t s) (GlamPile flt srt t r') (GlamPile flt srt t s')
glamPileBuilder (Z.Builder (Z.MkReq mkReq, Z.MkSpec mkSpc)) =
    Z.Builder (Z.MkReq mkReq', Z.MkSpec mkSpc')
  where
    mkReq' (GlamPile fc sc al ss) = GlamPile fc sc <$> traverse mkReq al <*> traverse mkReq ss
    mkSpc' (GlamPile fc sc al rs) = GlamPile fc sc <$> traverse mkSpc al <*> traverse mkSpc rs

glamPile :: (Traversable t, Z.MonadReactor m)
    => (flt -> s -> m Bool)
    -> (srt -> s -> s -> m Ordering)
    -> Z.Archetype m s c
    -> Z.Prototype m v (GlamPile flt srt t s) c
glamPile flt srt (Z.Archetype dis fin ini)
    = Z.Prototype
        (glamPileDisplay flt srt dis)
        (glamPileFinalizer fin)
        (glamPileInitializer ini)

-- | Sort the items on the listing given a sorting function
hdlGlamPileSortCriteria :: (Z.MonadReactor m)
    => Z.SceneHandler m v (GlamPile flt srt t a) srt ()
hdlGlamPileSortCriteria this@(Z.Obj ref its) f = lift $ do
    Z.doModifyIORef' ref $ \obj ->
        obj & (its.Z.model.field @"sortCriteria" .~ f)
            . (its.Z.model.field @"glamList" .~ []) -- this tells render to update displayItems
    Z.dirty this

-- | Filter the items on the listing given a filter function
hdlGlamPileFilterCriteria :: (Z.MonadReactor m)
    => Z.SceneHandler m v (GlamPile flt srt t a) flt ()
hdlGlamPileFilterCriteria this@(Z.Obj ref its) f = lift $ do
    Z.doModifyIORef' ref $ \obj ->
        obj & (its.Z.model.field @"filterCriteria" .~ f)
            . (its.Z.model.field @"glamList" .~ []) -- this tells render to update displayItems
    Z.dirty this

-- | lift a handler for a single widget into a handler of a list of widgets
-- where the input is broadcast to all the items in the list.
broadcastGlamPileHandler :: (Traversable t, Z.MonadReactor m)
    => Z.Handler m s a b
    -> Z.SceneHandler m v (GlamPile flt srt t s) a b
broadcastGlamPileHandler hdl = Z.magnifyScene (field @"rawPile") (W.broadcastPileHandler hdl)

glamPileDisplay :: (Foldable t, Z.MonadReactor m)
    => (flt -> s -> m Bool)
    -> (srt -> s -> s -> m Ordering)
    -> Z.Display m s ()
    -> Z.FrameDisplay m (GlamPile flt srt t s) ()
glamPileDisplay flt srt dis (_, GlamPile df ds ys xs) = do
    let toLi s = Z.bh "li" [] (dis s)
        df' = flt df
        ds' = srt ds
    ys' <- lift $ case ys of
            -- if displayList is empty, then run the filter and sort
            [] -> do
                let zs = toList xs
                zs' <- LM.filterMP df' zs
                zs'' <- LM.sortByM ds' zs'
                pure zs''
            -- else display as is
            ys' -> pure ys'
    Z.bh "ul" [] (fold $ toLi <$> ys')

glamPileFinalizer :: (Traversable t, Applicative m)
    => Z.Finalizer m s -> Z.Finalizer m (GlamPile flt srt t s)
glamPileFinalizer fin = (W.pileFinalizer fin) . rawPile

glamPileInitializer :: (Foldable t, Z.MonadReactor m)
    => Z.Initializer m s b
    -> Z.SceneInitializer m v (GlamPile flt srt t s) b
glamPileInitializer ini = Z.magnifyScene (field @"rawPile") (W.pileInitializer ini)
