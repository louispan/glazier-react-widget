{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Glazier.React.Framework.Core.Model where

import Control.Applicative.Esoteric
import qualified Control.Disposable as CD
import Control.Lens
import Control.Lens.Misc
import qualified Data.DList as DL
import Data.IORef
import qualified Data.Map.Strict as M
import qualified GHC.Generics as G
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import Glazier.Core
import Glazier.React
import qualified JavaScript.Extras as JE

newtype GadgetId = GadgetId { unGadgetId :: J.JSString }
    deriving (G.Generic, Ord, Eq)

-- | One for every archetype, may be shared for many prototypes
data Plan m = Plan
    { component :: ReactComponent
    , reactKey :: ReactKey
    , currentFrameNum :: Int
    , previousFrameNum :: Int
      -- things to dispose when this widget is removed
      -- cannot be hidden inside afterOnUpdated, as this needs to be used
      -- when finalizing
    , disposeOnRemoved :: CD.Disposable
    , disposeOnUpdated :: CD.Disposable -- ^ things to dispose on updated
    , everyOnUpdated :: m () -- ^ additional monadic action to take after every dirty
    , onceOnUpdated :: m () -- ^ additional monadic action to take after a dirty
    , onUpdated :: Maybe (J.Callback (J.JSVal -> IO ()))
    , onRender :: Maybe (J.Callback (IO J.JSVal))
    -- Storing listeners and refs in a 'M.Map', which simplifies the type of the model.
    -- Unfortunately, this means mismatches between event listeners and handlers
    -- are not compile time checked.
    -- Eg. `Glazier.React.Framework.Effect.MonadHTMLElement.focusRef` expects a ref
    -- but wont have compile error if `Glazier.React.Framework.Trigger.withRef` was not attached.
    -- The alternative is to store as a 'Many' in the model, but this ends up with messier types.
    , listeners :: M.Map GadgetId (DL.DList Listener)
    , refs :: M.Map GadgetId JE.JSRep
    } deriving (G.Generic)

makeLenses_ ''Plan

mkPlan :: MonadReactor m => J.JSString -> m (Plan m)
mkPlan n = Plan
    <$> doGetComponent
    <*> doMkReactKey n
    <*> pure 0 -- ^ currentFrameNum
    <*> pure 0 -- ^ previousFrameNum
    <*> pure mempty -- ^ disposeOnRemoved
    <*> pure mempty -- ^ disposeOnUpdated
    <*> pure (pure ()) -- ^ everyOnUpdated
    <*> pure (pure ()) -- ^ onceOnUpdated
    <*> pure Nothing -- ^ callback
    <*> pure Nothing -- ^ render
    <*> pure M.empty
    <*> pure M.empty

-- | A 'Frame' contains a widget 'Plan' as well as the model data.
data Frame m s = Frame
    { plan :: Plan m
    , model :: s
    } deriving (G.Generic, Functor)

makeLenses_ ''Frame

-- class HasPlan t m => HasFrame t m s | t -> m s where
--     _frame :: Lens' t (Frame m s)
--     _model :: Lens' t s
--     _model = (.) _frame _model
--     {-# INLINE _model #-}

-- instance HasFrame (Frame m s) m s where
--     _frame = id

--     _model f (Frame p a) = fmap (\ y -> Frame p y) (f a)
--     {-# INLINE _model #-}

-- instance HasPlan (Frame m s) m where
--     _plan = lens plan (\s a -> s { plan = a })

editFrame :: Lens' s' s -> Lens' (Frame m s') (Frame m s)
editFrame l = lens
    (\(Frame p s') -> Frame p (s' ^. l))
    (\(Frame _ s') (Frame p s) -> Frame p (s' & l .~ s))

-- | A 'Scene' is an mutable 'Obj' to a 'Frame'
-- In 'Glazier.React.Framework' we are using 'IORef'
type Scene p m s = Obj IORef p (Frame m s)
-- | 'Specimen' is the type used by 'Glazier.React.Framework.Core.Archetype'
-- It contains the same information as @Scene (Frame m s) m s@
-- and @IOObj (Frame m s) (Frame m s)@
-- that is an 'Obj' where the 'my' lens is 'id'.
type Specimen m s = IORef (Frame m s)

data Model m = Req | Spec m

-- | Helper function for defining types with teh same shape,
-- differing only if the leafs contains req or spec models.
-- From https://www.reddit.com/r/haskell/comments/6dh3ha/the_partial_options_monoid/
type family ModelType (mt :: Model (* -> *)) (s :: *) where
    ModelType 'Req s = s
    ModelType ('Spec m) s = IORef (Frame m s)

accessScene :: Lens' s' s -> Scene p m s' -> Scene p m s
accessScene l = access (editFrame l)

-- Add an action to run once after the next render
addOnceOnUpdated :: (MonadReactor m) => Scene p m s -> m () -> m ()
addOnceOnUpdated (Obj{..}) k = doModifyIORef' self (my._plan._onceOnUpdated %~ (^*> k))

-- Add an action to run after every render
addEveryOnUpdated :: (MonadReactor m) => Scene p m s -> m () -> m ()
addEveryOnUpdated (Obj{..}) k = doModifyIORef' self (my._plan._everyOnUpdated %~ (^*> k))
