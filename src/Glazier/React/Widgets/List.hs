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
{-# LANGUAGE DuplicateRecordFields #-}

module Glazier.React.Widgets.List
    ( Command(..)
    , Action(..)
    , AsAction(..)
    , Design(..)
    , HasDesign(..)
    , Plan(..)
    , HasPlan(..)
    , mkPlan
    , Model
    , Outline
    , Scene
    , Frame
    , Gizmo
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
    = RenderCommand (R.Gizmo (Model k itemWidget) Plan) [JE.Property] J.JSVal
    | DisposeCommand CD.SomeDisposable
    | MakerCommand (F (R.Maker (Action k itemWidget)) (Action k itemWidget))
    | ItemCommand k (R.CommandOf itemWidget)

data Action k itemWidget
    = ComponentRefAction J.JSVal
    | RenderAction
    | ComponentDidUpdateAction
    | DestroyItemAction k
    | MakeItemAction (k -> k) (k -> F (R.Maker (R.ActionOf itemWidget)) (R.ModelOf itemWidget))
    | AddItemAction k (R.GizmoOf itemWidget)
    | ItemAction k (R.ActionOf itemWidget)
    | SetFilterAction (R.OutlineOf itemWidget -> Bool)

type Model k itemWidget = Design k itemWidget R.WithGizmo
type Outline k itemWidget = Design k itemWidget R.WithOutline

data Design k itemWidget t = Design
    { _className :: J.JSString
    , _idx :: k
    , _items :: M.Map k (R.DesignType t itemWidget)
    , _itemsFilter :: R.OutlineOf itemWidget -> Bool
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
makeClassy ''Design
makeClassy ''Plan

mkPlan
    :: R.IsWidget itemWidget => R.ReactMlT Identity ()
    -> G.WindowT (R.SceneOf itemWidget) (R.ReactMlT Identity) ()
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
-- | Undecidable instances because itemWidget appears more often in hte constraint
-- but this is safe because @R.GizmoOf itemWidget@ is smaller than @Model k itemWidget@
instance (CD.Disposing (R.GizmoOf itemWidget)) =>
         CD.Disposing (Model k itemWidget) where
    disposing s = CD.DisposeList $ foldr ((:) . CD.disposing) [] (s ^. items)

-- Link Glazier.React.Model's HasPlan/HasModel with this widget's HasPlan/HasModel from makeClassy
instance HasPlan (R.Scene (Model k itemWidget) Plan) where
    plan = R.plan
instance HasDesign (R.Scene (Model k itemWidget) Plan) k itemWidget R.WithGizmo where
    design = R.model
instance HasPlan (R.Gizmo (Model k itemWidget) Plan) where
    plan = R.scene . plan
instance HasDesign (R.Gizmo (Model k itemWidget) Plan) k itemWidget R.WithGizmo where
    design = R.scene . design

type Scene k itemWidget = R.Scene (Model k itemWidget) Plan
type Frame k itemWidget = R.Frame (Model k itemWidget) Plan
type Gizmo k itemWidget = R.Gizmo (Model k itemWidget) Plan

type Widget k itemWidget = R.Widget (Command k itemWidget) (Action k itemWidget) (Outline k itemWidget) (Model k itemWidget) Plan

widget
    :: (R.IsWidget itemWidget, Ord k)
    => R.ReactMlT Identity ()
    -> itemWidget
    -> Widget k itemWidget
widget separator itemWidget = R.Widget
    (mkPlan separator (R.window itemWidget))
    window
    (gadget (R.mkGizmo itemWidget) (R.gadget itemWidget))

-- | Exposed to parent components to render this component
window :: G.WindowT (R.Scene (Model k itemWidget) Plan) (R.ReactMlT Identity) ()
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
    :: R.IsWidget itemWidget => R.ReactMlT Identity ()
    -> G.WindowT (R.SceneOf itemWidget) (R.ReactMlT Identity) ()
    -> G.WindowT (R.Scene (Model k itemWidget) Plan) (R.ReactMlT Identity) ()
render separator itemWindow = do
    s <- ask
    xs <- fmap (view R.scene) . filter ((s ^. itemsFilter) . R.outline . view R.model) . fmap snd .  M.toList <$> view items
    lift $ R.bh (JE.strJS "ul") [ ("key", s ^. key . to JE.toJS)
                                 , ("className", s ^. className . to JE.toJS)
                                 ] $ do
        let itemsWindows = (view G._WindowT itemWindow) <$> xs
            separatedWindows = DL.intersperse separator itemsWindows
        sequenceA_ separatedWindows

gadget
    :: (Ord k, R.IsWidget itemWidget)
    => (R.ModelOf itemWidget -> F (R.Maker (R.ActionOf itemWidget)) (R.GizmoOf itemWidget))
    -> G.GadgetT (R.ActionOf itemWidget) (R.GizmoOf itemWidget) Identity (D.DList (R.CommandOf itemWidget))
    -> G.GadgetT (Action k itemWidget) (R.Gizmo (Model k itemWidget) Plan) Identity (D.DList (Command k itemWidget))
gadget mkItemGizmo itemGadget = do
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
                itemGizmo <- MaybeT $ use (items . at k)
                deferredDisposables %= (`D.snoc` CD.disposing itemGizmo)
                -- Remove the todo from the model
                items %= M.delete k
                -- on re-render the todo Shim will not get rendered and will be removed by react
                D.singleton <$> (R.basicRenderCmd frameNum componentRef RenderCommand)
            maybe (pure mempty) pure ret

        MakeItemAction keyMaker itemModelMaker -> do
            n <- keyMaker <$> use idx
            idx .= n
            pure $ D.singleton $ MakerCommand $ do
                sm <- hoistF (R.mapAction $ \act -> ItemAction n act) (
                    itemModelMaker n >>= mkItemGizmo)
                pure $ AddItemAction n sm

        AddItemAction n v -> do
            items %= M.insert n v
            D.singleton <$> (R.basicRenderCmd frameNum componentRef RenderCommand)

        ItemAction k _ -> fmap (ItemCommand k) <$>
            (magnify (_ItemAction . to snd)
            (zoom (items . at k . _Just) itemGadget))

        SetFilterAction ftr -> do
            itemsFilter .= ftr
            D.singleton <$> (R.basicRenderCmd frameNum componentRef RenderCommand)
