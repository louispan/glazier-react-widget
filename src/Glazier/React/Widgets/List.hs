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
    , Schema(..)
    , HasSchema(..)
    , Plan(..)
    , HasPlan(..)
    , Outline
    , Model
    , Widget
    , widget
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
import qualified Glazier.React.Gadgets.Render as G.Render
import qualified Glazier.React.Gadgets.Dispose as G.Dispose
import qualified Glazier.React.Component as R
import qualified Glazier.React.Maker as R
import qualified Glazier.React.Markup as R
import qualified Glazier.React.Model as R
import qualified Glazier.React.Widget as R
import qualified JavaScript.Extras as JE

data Command k itemWidget
    = RenderCommand (G.Render.Command (R.Gizmo (Model k itemWidget) Plan))
    | DisposeCommand G.Dispose.Command
    | MakerCommand (F (R.Maker (Action k itemWidget)) (Action k itemWidget))
    | ItemCommand k (R.CommandOf itemWidget)

-- LOUISFIXME: data ListAction

data Action k itemWidget
    = RenderAction G.Render.Action
    | DisposeAction G.Dispose.Action
    | DestroyItemAction k
    | MakeItemAction (k -> k) (k -> F (R.Maker (R.ActionOf itemWidget)) (R.ModelOf itemWidget))
    | AddItemAction k (R.GizmoOf itemWidget)
    | ItemAction k (R.ActionOf itemWidget)
    | SetFilterAction (R.OutlineOf itemWidget -> Bool)

data Schema k itemWidget t = Schema
    { _className :: J.JSString
    , _idx :: k
    , _items :: M.Map k (R.Widget's t itemWidget)
    , _itemsFilter :: R.OutlineOf itemWidget -> Bool
    }

type Model k itemWidget = Schema k itemWidget R.GizmoType
type Outline k itemWidget = Schema k itemWidget R.OutlineType

instance R.IsWidget itemWidget => R.ToOutline (Model k itemWidget) (Outline k itemWidget) where
    outline (Schema a b c d) = Schema a b (R.outline <$> c) d

mkModel :: R.IsWidget itemWidget => itemWidget -> Outline k itemWidget -> F (R.Maker (Action k itemWidget)) (Model k itemWidget)
mkModel w (Schema a b c d) = Schema
    <$> pure a
    <*> pure b
    <*> M.traverseWithKey (\k i -> R.hoistWithAction (ItemAction k) (R.mkGizmo' w i)) c
    <*> pure d

data Plan = Plan
    { _renderPlan :: G.Render.Plan
    , _disposePlan :: G.Dispose.Plan
    , _component :: R.ReactComponent
    , _key :: J.JSString
    , _onRender ::  J.Callback (J.JSVal -> IO J.JSVal)
    } deriving (G.Generic)

makeClassyPrisms ''Action
makeClassy ''Schema
makeClassy ''Plan

mkPlan
    :: R.IsWidget itemWidget => R.ReactMlT Identity ()
    -> G.WindowT (R.SceneOf itemWidget) (R.ReactMlT Identity) ()
    -> R.Frame (Model k itemWidget) Plan
    -> F (R.Maker (Action k itemWidget)) Plan
mkPlan separator itemWindow frm = Plan
    <$> (R.hoistWithAction RenderAction G.Render.mkPlan)
    <*> (R.hoistWithAction DisposeAction G.Dispose.mkPlan)
    <*> R.getComponent
    <*> R.mkKey
    <*> (R.mkRenderer frm $ const (render separator itemWindow))

instance CD.Disposing Plan
-- | Undecidable instances because itemWidget appears more often in the constraint
-- but this is safe because @R.GizmoOf itemWidget@ is smaller than @Model k itemWidget@
instance (CD.Disposing (R.GizmoOf itemWidget)) =>
         CD.Disposing (Model k itemWidget) where
    disposing s = CD.DisposeList $ foldr ((:) . CD.disposing) [] (s ^. items)

-- Link Glazier.React.Model's HasPlan/HasModel with this widget's HasPlan/HasModel from makeClassy
instance HasPlan (R.Scene (Model k itemWidget) Plan) where
    plan = R.plan
instance HasSchema (R.Scene (Model k itemWidget) Plan) k itemWidget R.GizmoType where
    schema = R.model
instance HasPlan (R.Gizmo (Model k itemWidget) Plan) where
    plan = R.scene . plan
instance HasSchema (R.Gizmo (Model k itemWidget) Plan) k itemWidget R.GizmoType where
    schema = R.scene . schema

instance G.Render.HasPlan (R.Scene (Model k itemWidget) Plan) where
    plan = R.plan . renderPlan
instance G.Render.HasPlan (R.Gizmo (Model k itemWidget) Plan) where
    plan = R.scene . G.Render.plan

instance G.Dispose.HasPlan (R.Scene (Model k itemWidget) Plan) where
    plan = R.plan . disposePlan
instance G.Dispose.HasPlan (R.Gizmo (Model k itemWidget) Plan) where
    plan = R.scene . G.Dispose.plan

type Widget k itemWidget = R.Widget (Action k itemWidget) (Outline k itemWidget) (Model k itemWidget) Plan (Command k itemWidget)
widget
    :: (R.IsWidget itemWidget, Ord k)
    => R.ReactMl ()
    -> itemWidget
    -> Widget k itemWidget
widget separator itemWidget = R.Widget
    (mkModel itemWidget)
    (mkPlan separator (R.window itemWidget))
    window
    (gadget (R.mkGizmo itemWidget) (R.gadget itemWidget))

-- | Exposed to parent components to render this component
window :: G.WindowT (R.Scene (Model k itemWidget) Plan) R.ReactMl ()
window = do
    s <- ask
    lift $ R.lf (s ^. component . to JE.toJS')
        [ ("key",  s ^. key . to JE.toJS')
        , ("render", s ^. onRender . to JE.toJS')
        -- LOUISFIXME: How ot make sure we don't forget to attach these listeners?
        , ("ref", s ^. G.Render.onComponentRef . to JE.toJS')
        , ("componentDidUpdate", s ^. G.Dispose.onComponentDidUpdate . to JE.toJS')
        ]

-- | Internal rendering used by the React render callback
render
    :: R.IsWidget itemWidget => R.ReactMlT Identity ()
    -> G.WindowT (R.SceneOf itemWidget) R.ReactMl ()
    -> G.WindowT (R.Scene (Model k itemWidget) Plan) R.ReactMl ()
render separator itemWindow = do
    s <- ask
    xs <- fmap (view R.scene) . filter ((s ^. itemsFilter) . R.outline . view R.model) . fmap snd .  M.toList <$> view items
    lift $ R.bh "ul" [ ("key", s ^. key . to JE.toJS')
                     , ("className", s ^. className . to JE.toJS')
                     ] $ do
        let itemsWindows = (view G._WindowT itemWindow) <$> xs
            separatedWindows = DL.intersperse separator itemsWindows
        sequenceA_ separatedWindows

gadget
    :: (Ord k, R.IsWidget itemWidget)
    => (R.ModelOf itemWidget -> F (R.Maker (R.ActionOf itemWidget)) (R.GizmoOf itemWidget))
    -> G.Gadget (R.ActionOf itemWidget) (R.GizmoOf itemWidget) (D.DList (R.CommandOf itemWidget))
    -> G.Gadget (Action k itemWidget) (R.Gizmo (Model k itemWidget) Plan) (D.DList (Command k itemWidget))
gadget mkItemGizmo itemGadget = do
    a <- ask
    case a of
        RenderAction _ -> fmap RenderCommand <$> magnify _RenderAction G.Render.gadget

        DisposeAction _ -> fmap DisposeCommand <$> magnify _DisposeAction G.Dispose.gadget

        DestroyItemAction k -> do
            -- queue up callbacks to be released after rerendering
            ret <- runMaybeT $ do
                itemGizmo <- MaybeT $ use (items . at k)
                G.Dispose.deferredDisposables %= (`D.snoc` CD.disposing itemGizmo)
                -- Remove the todo from the model
                items %= M.delete k
                -- on re-render the todo Shim will not get rendered and will be removed by react
                lift renderGadget
            maybe (pure mempty) pure ret

        MakeItemAction keyMaker itemModelMaker -> do
            n <- keyMaker <$> use idx
            idx .= n
            pure $ D.singleton $ MakerCommand $ do
                sm <- R.hoistWithAction (ItemAction n) (
                    itemModelMaker n >>= mkItemGizmo)
                pure $ AddItemAction n sm

        AddItemAction n v -> do
            items %= M.insert n v
            renderGadget

        ItemAction k _ -> fmap (ItemCommand k) <$>
            (magnify (_ItemAction . to snd)
            (zoom (items . at k . _Just) itemGadget))

        SetFilterAction ftr -> do
            itemsFilter .= ftr
            renderGadget

renderGadget :: G.Gadget a (R.Gizmo (Model k itemWidget) Plan) (D.DList (Command k itemWidget))
renderGadget = fmap RenderCommand <$> G.withGadgetT G.Render.RenderAction G.Render.gadget
