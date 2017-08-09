{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Glazier.React.Framework.Widget
  ( WidgetCommand(..)
  , WidgetAction(..)
  , HasProperties(..)
  , HasWidgetPlan(..)
  , HasDetails(..)
  , HasPlans(..)
  , Design(..)
  , _Design
  , Entity
  , withTVar
  , inTVar
  , renderGadget
  , widgetGadget
  , widgetWindow
  , mkEntity
  ) where
-- We want to hide mkWidgetPlan

import Control.Applicative
import Control.Concurrent.STM
import Control.Lens
import Control.Monad.Free.Church
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Diverse.Lens
import qualified Data.DList as D
import Data.Kind
import qualified Data.Map.Strict as M
import Data.Semigroup
import qualified GHC.Generics as G
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Foreign.Callback.Internal as J
import qualified GHCJS.Types as J
import qualified Glazier as G
import qualified Glazier.React as R
import qualified JavaScript.Extras as JE

data WidgetCommand
    = RenderCommand [JE.Property] J.JSVal
    | DisposeCommand (R.Disposable ())

data WidgetAction
    = ComponentRefAction J.JSVal
    | RenderAction
    | DisposeAction
----------------------------------------------------------

class HasProperties c where
    properties :: Lens' c [JE.Property]

instance HasProperties [JE.Property] where
    properties = id

----------------------------------------------------------

-- | WidgetPlan has to be stored differently to other plans because mkWidgetPlan needs
-- additional parameters
data WidgetPlan = WidgetPlan
    { _key :: J.JSString
    , _frameNum :: Int
    , _component :: R.ReactComponent
    , _componentRef :: J.JSVal
    , _deferredDisposables :: R.Disposable ()
    , _onRender ::  J.Callback (IO J.JSVal)
    , _onComponentRef :: J.Callback (J.JSVal -> IO ())
    , _onComponentDidUpdate :: J.Callback (J.JSVal -> IO ())
    , _listeners :: [R.Listener]
    } deriving (G.Generic)

makeClassy ''WidgetPlan

-- | NB. This createsa a dummy onRender callback!
mkWidgetPlan
    :: UniqueMember WidgetAction acts
    => [(J.JSString, J.JSVal -> MaybeT IO [Which acts])]
    -> F (R.Reactor (Which acts)) WidgetPlan
mkWidgetPlan hls = R.hoistWithAction pick (WidgetPlan
    <$> R.mkKey' -- key
    <*> pure 0 -- frameNum
    <*> R.getComponent -- component
    <*> pure J.nullRef -- componentRef
    <*> pure mempty -- deferredDisposables
    <*> pure (J.Callback J.nullRef) -- onRender (dummy for now)
    <*> (R.mkHandler $ pure . pure . ComponentRefAction) -- onComponentRef
    <*> (R.mkHandler $ pure . pure . const DisposeAction) -- onComponentDidUpdate
    )
    <*> (traverse go . M.toList . M.fromListWith (liftA2 combine) $ hls) -- triggers
  where
    combine f g = ((<>) <$> f <*> g) <|> f <|> g -- combine all results that succeed
    go (n, f) = (\a -> (n, a)) <$> R.mkHandler f

instance R.Dispose WidgetPlan

----------------------------------------------------------

class HasDetails c dtls | c -> dtls where
    details :: Lens' c (Many dtls)

class HasPlans c plns | c -> plns where
    plans :: Lens' c (Many plns)

----------------------------------------------------------

newtype Design (dtls :: [Type]) (plns :: [Type]) = Design
    { getDesign ::
        ( [JE.Property]
        , Many dtls
        , WidgetPlan
        , Many plns
        )
    }
    deriving G.Generic

instance (R.Dispose (Many dtls), R.Dispose (Many plns)) => R.Dispose (Design dtls plns)

instance HasProperties (Design dtls plns) where
    properties = _Design . _1

instance HasDetails (Design dtls plns) dtls where
    details = _Design . _2

instance HasWidgetPlan (Design dtls plns) where
    widgetPlan = _Design . _3

instance HasPlans (Design dtls plns) plns where
    plans = _Design . _4

_Design :: Iso
    (Design dtls plns)
    (Design dtls' plns')
    ([JE.Property], Many dtls, WidgetPlan, Many plns)
    ([JE.Property], Many dtls', WidgetPlan, Many plns')
_Design = iso getDesign Design

----------------------------------------------------------

-- type Entity dtls plns = F.Shared (Design dtls plns)
type Entity dtls plns = TVar (Design dtls plns)

----------------------------------------------------------

renderGadget :: UniqueMember WidgetCommand cmds => G.GadgetT acts (Design dtls plns) STM (D.DList (Which cmds))
renderGadget = do
    -- Just change the state to a different number so the React PureComponent will call render()
    (widgetPlan . frameNum) %= (\i -> (i `mod` JE.maxSafeInteger) + 1)
    i <- JE.toJS <$> use (widgetPlan . frameNum)
    r <- use (widgetPlan . componentRef)
    pure . D.singleton . pick $ RenderCommand [("frameNum", JE.JSVar i)] r

widgetGadget :: G.GadgetT WidgetAction (Design dtls plns) STM (D.DList WidgetCommand)
widgetGadget = do
    a <- ask
    case a of
        ComponentRefAction node -> do
            (widgetPlan . componentRef) .= node
            pure mempty

        RenderAction -> do
            -- Just change the state to a different number so the React PureComponent will call render()
            (widgetPlan . frameNum) %= (\i -> (i `mod` JE.maxSafeInteger) + 1)
            i <- JE.toJS <$> use (widgetPlan . frameNum)
            r <- use (widgetPlan . componentRef)
            pure . D.singleton $ RenderCommand [("frameNum", JE.JSVar i)] r

        DisposeAction -> do
            -- Run delayed commands that need to wait until frame is re-rendered
            -- Eg focusing after other rendering changes
            ds <- use (widgetPlan . deferredDisposables)
            (widgetPlan . deferredDisposables) .= mempty
            pure . D.singleton . DisposeCommand $ ds

withTVar :: TVar s -> G.GadgetT a s STM c -> G.WindowT a STM c
withTVar v' = review G._WRMT' . go v' . view G._GRMST'
  where
    go :: TVar s -> (a -> s -> STM (Maybe c, s)) -> a -> STM (Maybe c)
    go v f a = do
        s <- readTVar v
        (c, s') <- f a s
        writeTVar v s'
        pure c

inTVar :: (Monad (t STM), MonadTrans t) => G.WindowT s (t STM) c -> G.WindowT (TVar s) (t STM) c
inTVar w = review G._WRMT' $ \s -> do
        s' <- lift $ readTVar s
        view G._WRMT' w s'

widgetWindow :: G.WindowT (Design dtls plns) (R.ReactMlT STM) ()
widgetWindow = do
    s <- ask
    lift $
        R.lf
            (s ^. widgetPlan  . component . to JE.toJS')
            [ ("ref", s ^. widgetPlan . onComponentRef)
            , ("componentDidUpdate", s ^. widgetPlan . onComponentDidUpdate)
            ]
            [ ("key", s ^. widgetPlan . key . to JE.toJS')
            -- NB. render is a JE.Property, not a 'R.Listener' as it returns an 'IO JSVal'
            , ("render", s ^. widgetPlan . onRender . to JE.toJS')
            ]

----------------------------------------------------------

-- | Make a Entity given the Detail, where the Model type is
-- a basic tuple of Detail and Plan.
mkEntity
    :: UniqueMember WidgetAction acts
    => [JE.Property]
    -> Many dtls
    -> [(J.JSString, J.JSVal -> MaybeT IO [Which acts])]
    -> F (R.Reactor (Which acts)) (Many plns)
    -> G.WindowT (Entity dtls plns) (R.ReactMlT STM) ()
    -> F (R.Reactor (Which acts)) (Entity dtls plns)
mkEntity ps dtls hls mkPlns render = do
    mdl <- (\plns compPln -> Design (ps, dtls, plns, compPln)) <$> mkWidgetPlan hls <*> mkPlns
    v <- R.mkTVar mdl
    rnd <- R.mkRenderer render v
    R.changeTVar v ((widgetPlan . onRender) .~ rnd)
    pure v
