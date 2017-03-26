{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Widgets.List
    ( Command(..)
    , Action(..)
    , AsAction(..)
    , Plan(..)
    , HasPlan(..)
    , mkPlan
    , Model(..)
    , HasModel(..)
    , Design
    , Frame
    , SuperModel
    , Widget
    , widget
    , window
    , gadget
    ) where

import qualified Control.Disposable as CD
import Control.Lens
import Control.Monad.Free.Church
import Control.Monad.Morph
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import qualified Data.DList as D
import Data.Foldable
import qualified Data.JSString as J
import qualified Data.List as DL
import qualified Data.Map.Strict as M
import qualified GHC.Generics as G
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import qualified Glazier as G
import qualified Glazier.React.Command as R
import qualified Glazier.React.Component as R
import qualified Glazier.React.Maker as R
import qualified Glazier.React.Markup as R
import qualified Glazier.React.Model as R
import qualified Glazier.React.Widget as R
import qualified JavaScript.Extras as JE

data Command k itemWidget
    = RenderCommand (R.SuperModel (Model k itemWidget) Plan) [JE.Property] J.JSVal
    | DisposeCommand CD.SomeDisposable
    | MakerCommand (F (R.Maker (Action k itemWidget)) (Action k itemWidget))
    | ItemCommand k (R.CommandOf itemWidget)

data Action k itemWidget
    = ComponentRefAction J.JSVal
    | RenderAction
    | ComponentDidUpdateAction
    | DestroyItemAction k
    | MakeItemAction (k -> k) (k -> F (R.Maker (R.ActionOf itemWidget)) (R.ModelOf itemWidget))
    | AddItemAction k (R.SuperModelOf itemWidget)
    | ItemAction k (R.ActionOf itemWidget)
    | SetFilterAction (R.SuperModelOf itemWidget -> Bool)

data Model k itemWidget = Model
    { _className ::J.JSString
    , _itemKey :: k
    , _itemsModel :: M.Map k (R.SuperModelOf itemWidget)
    , _itemsFilter :: R.SuperModelOf itemWidget -> Bool
    }

data Plan = Plan
    { _component :: R.ReactComponent
    , _key :: J.JSString
    , _frameNum :: Int
    , _componentRef :: J.JSVal
    , _deferredDisposables :: D.DList CD.SomeDisposable
    , _onRender ::  J.Callback (J.JSVal -> IO J.JSVal)
    , _onComponentRef :: J.Callback (J.JSVal -> IO ())
    , _onComponentDidUpdate :: J.Callback (J.JSVal -> IO ())
    } deriving (G.Generic)

makeClassyPrisms ''Action
makeClassy ''Plan
makeClassy ''Model

mkPlan
    :: R.ReactMlT Identity ()
    -> G.WindowT (R.DesignOf itemWidget) (R.ReactMlT Identity) ()
    -> R.Frame (Model k itemWidget) Plan
    -> F (R.Maker (Action k itemWidget)) Plan
mkPlan separator itemWindow frm = Plan
    <$> R.getComponent
    <*> R.mkKey
    <*> pure 0
    <*> pure J.nullRef
    <*> pure mempty
    <*> (R.mkRenderer frm $ const (render separator itemWindow))
    <*> (R.mkHandler $ pure . pure . ComponentRefAction)
    <*> (R.mkHandler $ pure . pure . const ComponentDidUpdateAction)

instance CD.Disposing Plan
instance (CD.Disposing (R.SuperModelOf itemWidget)) =>
         CD.Disposing (Model k itemWidget) where
    disposing s = CD.DisposeList $ foldr ((:) . CD.disposing) [] (_itemsModel s)

-- Link Glazier.React.Model's HasPlan/HasModel with this widget's HasPlan/HasModel from makeClassy
instance HasPlan (R.Design (Model k itemWidget) Plan) where
    plan = R.plan
instance HasModel (R.Design (Model k itemWidget) Plan) k itemWidget where
    model = R.model
instance HasPlan (R.SuperModel (Model k itemWidget) Plan) where
    plan = R.design . plan
instance HasModel (R.SuperModel (Model k itemWidget) Plan) k itemWidget where
    model = R.design . model

type Design k itemWidget = R.Design (Model k itemWidget) Plan
type Frame k itemWidget = R.Frame (Model k itemWidget) Plan
type SuperModel k itemWidget = R.SuperModel (Model k itemWidget) Plan

type Widget k itemWidget = R.Widget (Command k itemWidget) (Action k itemWidget) (Model k itemWidget) Plan
widget
    :: (R.IsWidget itemWidget, Ord k)
    => R.ReactMlT Identity ()
    -> itemWidget
    -> R.Widget (Command k itemWidget) (Action k itemWidget) (Model k itemWidget) Plan
widget separator itemWidget = R.Widget
    (mkPlan separator (R.window itemWidget))
    window
    (gadget (R.mkSuperModel itemWidget) (R.gadget itemWidget))

-- | Exposed to parent components to render this component
window :: G.WindowT (R.Design (Model k itemWidget) Plan) (R.ReactMlT Identity) ()
window = do
    s <- ask
    lift $ R.lf (s ^. component . to JE.toJS)
        [ ("key",  s ^. key . to JE.toJS)
        , ("render", s ^. onRender . to JE.toJS)
        , ("ref", s ^. onComponentRef . to JE.toJS)
        , ("componentDidUpdate", s ^. onComponentDidUpdate . to JE.toJS)
        ]

-- | Internal rendering used by the React render callback
render
    :: R.ReactMlT Identity ()
    -> G.WindowT (R.DesignOf itemWidget) (R.ReactMlT Identity) ()
    -> G.WindowT (R.Design (Model k itemWidget) Plan) (R.ReactMlT Identity) ()
render separator itemWindow = do
    s <- ask
    items <- fmap (view R.design) . filter (s ^. itemsFilter) . fmap snd .  M.toList <$> view itemsModel
    lift $ R.bh (JE.strJS "ul") [ ("key", s ^. key . to JE.toJS)
                                 , ("className", s ^. className . to JE.toJS)
                                 ] $ do
        let itemsWindows = (view G._WindowT itemWindow) <$> items
            separatedWindows = DL.intersperse separator itemsWindows
        sequenceA_ separatedWindows

gadget
    :: (Ord k, R.IsWidget itemWidget)
    => (R.ModelOf itemWidget -> F (R.Maker (R.ActionOf itemWidget)) (R.SuperModelOf itemWidget))
    -> G.GadgetT (R.ActionOf itemWidget) (R.SuperModelOf itemWidget) Identity (D.DList (R.CommandOf itemWidget))
    -> G.GadgetT (Action k itemWidget) (R.SuperModel (Model k itemWidget) Plan) Identity (D.DList (Command k itemWidget))
gadget mkItemSuperModel itemGadget = do
    a <- ask
    case a of
        ComponentRefAction node -> do
            componentRef .= node
            pure mempty

        RenderAction ->
            D.singleton <$> (R.basicRenderCmd frameNum componentRef RenderCommand)

        ComponentDidUpdateAction -> do
            -- Run delayed commands that need to wait until frame is re-rendered
            -- Eg focusing after other rendering changes
            ds <- use deferredDisposables
            deferredDisposables .= mempty
            pure . D.singleton . DisposeCommand . CD.DisposeList $ D.toList ds

        DestroyItemAction k -> do
            -- queue up callbacks to be released after rerendering
            ret <- runMaybeT $ do
                itemSuperModel <- MaybeT $ use (itemsModel . at k)
                deferredDisposables %= (`D.snoc` CD.disposing itemSuperModel)
                -- Remove the todo from the model
                itemsModel %= M.delete k
                -- on re-render the todo Shim will not get rendered and will be removed by react
                D.singleton <$> (R.basicRenderCmd frameNum componentRef RenderCommand)
            maybe (pure mempty) pure ret

        MakeItemAction keyMaker itemModelMaker -> do
            n <- keyMaker <$> use itemKey
            itemKey .= n
            pure $ D.singleton $ MakerCommand $ do
                sm <- hoistF (R.mapAction $ \act -> ItemAction n act) (
                    itemModelMaker n >>= mkItemSuperModel)
                pure $ AddItemAction n sm

        AddItemAction n v -> do
            itemsModel %= M.insert n v
            D.singleton <$> (R.basicRenderCmd frameNum componentRef RenderCommand)

        ItemAction k _ -> fmap (ItemCommand k) <$>
            (magnify (_ItemAction . to snd)
            (zoom (itemsModel . at k . _Just) itemGadget))

        SetFilterAction ftr -> do
            itemsFilter .= ftr
            D.singleton <$> (R.basicRenderCmd frameNum componentRef RenderCommand)
