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

module Glazier.React.Widget.Listing where

import Control.Lens
import Control.Monad.Trans.Cont
import Data.Foldable
import qualified Data.Foldable.Esoteric as E
import qualified Glazier.React.Framework as R

-- | Converts a builder with a plan of @[a]@ to a plan of @Listing a@
listingBuilder :: (Applicative m, Traversable t)
    => R.Builder m r s r s
    -> R.Builder m (t r) (t s) (t r) (t s)
listingBuilder (R.Builder (R.MkReq mkReq, R.MkSpec mkSpc)) =
    R.Builder (R.MkReq mkReq', R.MkSpec mkSpc')
  where
    mkReq' ss = traverse mkReq ss
    mkSpc' rs = traverse mkSpc rs

-- | This version drops the original item handlers @a -> b@, and only have list handlers.
listing :: (R.MonadReactor m, Traversable t)
    => R.Archetype m s c
    -> R.Prototype m v (t s) c
listing (R.Archetype dis fin ini)
    = R.Prototype
        (listingDisplay dis)
        (\ss -> fold <$> traverse fin ss)
        (listingInitializer ini)

-- | lift a handler for a single widget into a handler of a list of widgets
-- where the input is broadcast to all the items in the list.
broadcastHandler :: (R.MonadReactor m, Foldable t)
    => R.Handler m s a b
    -> R.SceneHandler m v (t s)
        a b
broadcastHandler hdl (R.Obj ref its) a = ContT $ \fire -> do
    obj <- R.doReadIORef ref
    E.traverse_' (\s -> runContT (hdl s a) fire) (obj ^. its.R.model)

listingDisplay :: (R.MonadReactor m, Functor t, Foldable t)
    => R.Display m s ()
    -> R.FrameDisplay m (t s) ()
listingDisplay dis (_, ss) = do
    let toLi s = R.bh "li" [] (dis s)
    R.bh "ul" [] (fold $ toLi <$> ss)

listingInitializer :: (R.MonadReactor m, Foldable t)
    => R.Initializer m s b
    -> R.SceneInitializer m v (t s) b
listingInitializer ini (R.Obj ref its) = ContT $ \k -> do
    obj <- R.doReadIORef ref
    E.traverse_' (\s -> runContT (ini s) k) (obj ^. its.R.model)
