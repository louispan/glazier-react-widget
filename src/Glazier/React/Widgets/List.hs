{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE MonomorphismRestriction #-}

module Glazier.React.Widgets.List where
    -- ( Command(..)
    -- , Command'(..)
    -- , Action(..)
    -- , AsAction(..)
    -- , Action'(..)
    -- , AsAction'(..)
    -- , Schema(..)
    -- , HasSchema(..)
    -- , Plan(..)
    -- , HasPlan(..)
    -- , Outline
    -- , Model
    -- , Widget
    -- , widget
    -- ) where

import Control.Applicative
import Control.Concurrent.MVar
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
import qualified Glazier as G
import qualified Glazier.React as R
import qualified Glazier.React.Commands.Maker as C.Maker
import qualified Glazier.React.Devices.Render as D.Render
import qualified Glazier.React.Devices.Dispose as D.Dispose
import qualified Glazier.React.Displays.Component as D.Component
import qualified JavaScript.Extras as JE

-- -- | List specific command
-- data Command' k itemWidget = ItemCommand k (R.CommandOf itemWidget)

-- -- | Combined Command
-- data Command k itemWidget
--     = RenderCommand (D.Render.Command (R.Shared (Model k itemWidget) Plan))
--     | DisposeCommand D.Dispose.Command
--     | MakerCommand (C.Maker.Command (Action k itemWidget))
--     | ListCommand (Command' k itemWidget)

-- -- | List specific actions
-- data Action' k itemWidget
--     = DestroyItemAction k
--     | MakeItemAction (k -> k) (k -> F (R.Maker (R.ActionOf itemWidget)) (R.ModelOf itemWidget))
--     | AddItemAction k (R.GizmoOf itemWidget)
--     | ItemAction k (R.ActionOf itemWidget)
--     | SetFilterAction (R.OutlineOf itemWidget -> Bool)

-- -- | Combined Action
-- data Action k itemWidget
--     = RenderAction G.Render.Action
--     | DisposeAction G.Dispose.Action
--     | ListAction (Action' k itemWidget)

data Schema k itemWidget (p :: R.Part) = Schema
    { _className :: J.JSString
    , _idx :: k
    , _items :: M.Map k (R.Widget's p itemWidget)
    , _itemsFilter :: R.OutlineOf itemWidget -> Bool
    }

type Detail k itemWidget = Schema k itemWidget 'R.Entity'
type Outline k itemWidget = Schema k itemWidget 'R.Outline'

-- Top-level binding with no type signature:
--   outline :: forall t (t1 :: R.Part) k itemWidget (p :: R.Part) w.
--              (R.OutlineOf itemWidget ~ R.OutlineOf t,
--               R.OutlineOf w ~ R.Widget's p itemWidget,
--               R.HasIVal (R.Widget's t1 t) (R.ModelOf w), R.HasDetail w,
--               R.ToOutline w) =>
--              w -> Schema k t t1 -> Schema k itemWidget p

outline :: (R.HasDetail itemWidget, R.ToOutline itemWidget) => itemWidget -> Detail k itemWidget -> Outline k itemWidget
outline w (Schema a b c d) =
        Schema a b ((view (R.ival . R.detail w . to (R.outline w))) <$> c) d

-- mkDetail :: R.IsWidget itemWidget => itemWidget -> Outline k itemWidget -> F (R.Maker (Action k itemWidget)) (Model k itemWidget)
-- mkDetail w (Schema a b c d) = Schema
--     <$> pure a
--     <*> pure b
--     <*> M.traverseWithKey (\k i -> R.hoistWithAction (ListAction . ItemAction k) (R.mkGizmo' w i)) c
--     <*> pure d

data Plan = Plan
    { _componentPlan :: D.Component.Plan
    , _renderPlan :: D.Render.Plan
    , _disposePlan :: D.Dispose.Plan
    } deriving (G.Generic)

-- makeClassyPrisms ''Action
-- makeClassyPrisms ''Action'
-- makeClassy ''Schema
-- makeClassy ''Plan

-- mkPlan
--     :: (R.HasScene scn (Model k itemWidget) Plan, R.IsWidget itemWidget)
--     => R.ReactMlT Identity ()
--     -> G.WindowT (R.SceneOf itemWidget) (R.ReactMlT Identity) ()
--     -> (scn -> R.RenderProps)
--     -> MVar scn
--     -> F (R.Maker (Action k itemWidget)) Plan
-- mkPlan separator itemWindow renderProps frm = Plan
--     <$> (WComponent.mkPlan (render separator itemWindow renderProps) frm)
--     <*> (R.hoistWithAction RenderAction G.Render.mkPlan)
--     <*> (R.hoistWithAction DisposeAction G.Dispose.mkPlan)

instance CD.Disposing Plan

-- -- | Undecidable instances because itemWidget appears more often in the constraint
-- -- but this is safe because @R.GizmoOf itemWidget@ is smaller than @Model k itemWidget@
-- instance (CD.Disposing (R.GizmoOf itemWidget)) =>
--          CD.Disposing (Model k itemWidget) where
--     disposing s = CD.DisposeList $ foldr ((:) . CD.disposing) [] (s ^. items)

-- -- Link Glazier.React.Model's HasPlan/HasModel with this widget's HasPlan/HasModel from makeClassy
-- instance HasPlan pln => HasPlan (R.Scene (Model k itemWidget) pln) where
--     plan = R.plan . plan
-- instance HasPlan pln => HasPlan (R.Gizmo (Model k itemWidget) pln) where
--     plan = R.plan . plan
-- instance HasSchema mdl k itemWidget R.GizmoType =>
--          HasSchema (R.Scene mdl pln) k itemWidget R.GizmoType where
--     schema = R.model . schema
-- instance HasSchema mdl k itemWidget R.GizmoType =>
--          HasSchema (R.Gizmo mdl pln) k itemWidget R.GizmoType where
--     schema = R.scene . schema

-- -- link the HasPlan for the composites
-- instance WComponent.HasPlan Plan where
--     plan = componentPlan
-- instance G.Render.HasPlan Plan where
--     plan = renderPlan
-- instance G.Dispose.HasPlan Plan where
--     plan = disposePlan

-- type Widget k itemWidget = R.Widget (Action k itemWidget) (Outline k itemWidget) (Model k itemWidget) Plan (Command k itemWidget)
-- widget
--     :: (R.IsWidget itemWidget, Ord k)
--     => R.ReactMl ()
--     -> itemWidget
--     -> (forall scn. R.HasScene scn (Model k itemWidget) Plan => scn -> R.WindowProps)
--     -> (forall scn. R.HasScene scn (Model k itemWidget) Plan => scn -> R.RenderProps)
--     -> Widget k itemWidget
-- widget separator itemWidget windowProps renderProps = R.Widget
--     (mkModel itemWidget)
--     (mkPlan separator (R.window itemWidget) renderProps)
--     (window windowProps)
--     (gadget (R.mkGizmo itemWidget) (R.gadget itemWidget))

-- -- | Exposed to parent components to render this component
-- window :: R.HasScene scn (Model k itemWidget) Plan => (scn -> R.WindowProps) -> G.WindowT scn R.ReactMl ()
-- window windowProps = WComponent.window $ fold [windowProps, G.Render.windowProps, G.Dispose.windowProps]

-- -- | Internal rendering used by the React render callback
-- render
--     :: (R.HasScene scn (Model k itemWidget) Plan, R.IsWidget itemWidget)
--     => R.ReactMlT Identity ()
--     -> G.WindowT (R.SceneOf itemWidget) R.ReactMl ()
--     -> (scn -> R.RenderProps)
--     -> G.WindowT scn R.ReactMl ()
-- render separator itemWindow renderProps = do
--     s <- ask
--     let R.RenderProps (props, hdls) = renderProps s
--     xs <-
--         fmap (view R.scene) .
--         filter ((s ^. R.scene . itemsFilter) . R.outline . view R.model) .
--         fmap snd .
--         M.toList <$>
--         view (R.scene . items)
--     lift $
--         R.bh
--             "ul"
--             ([ ("key", s ^. R.scene . WComponent.key . to JE.toJS')
--             , ("className", s ^. R.scene . className . to JE.toJS')
--             ] ++ props)
--             hdls $ do
--         let itemsWindows = view G._WindowT itemWindow <$> xs
--             separatedWindows = DL.intersperse separator itemsWindows
--         sequenceA_ separatedWindows

-- gadget
--     :: (Ord k, R.IsWidget itemWidget)
--     => (R.ModelOf itemWidget -> F (R.Maker (R.ActionOf itemWidget)) (R.GizmoOf itemWidget))
--     -> G.Gadget (R.ActionOf itemWidget) (R.GizmoOf itemWidget) (D.DList (R.CommandOf itemWidget))
--     -> G.Gadget (Action k itemWidget) (R.Gizmo (Model k itemWidget) Plan) (D.DList (Command k itemWidget))
-- gadget mkItemGizmo itemGadget =
--         (fmap RenderCommand <$> magnify _RenderAction G.Render.gadget)
--     <|> (fmap DisposeCommand <$> magnify _DisposeAction G.Dispose.gadget)
--     <|> (magnify _ListAction (listGadget mkItemGizmo itemGadget))

-- listGadget
--     :: (Ord k, R.IsWidget itemWidget)
--     => (R.ModelOf itemWidget -> F (R.Maker (R.ActionOf itemWidget)) (R.GizmoOf itemWidget))
--     -> G.Gadget (R.ActionOf itemWidget) (R.GizmoOf itemWidget) (D.DList (R.CommandOf itemWidget))
--     -> G.Gadget (Action' k itemWidget) (R.Gizmo (Model k itemWidget) Plan) (D.DList (Command k itemWidget))
-- listGadget mkItemGizmo itemGadget = do
--     a <- ask
--     case a of
--         DestroyItemAction k -> do
--             -- queue up callbacks to be released after rerendering
--             ret <- runMaybeT $ do
--                 itemGizmo <- MaybeT $ use (items . at k)
--                 G.Dispose.deferredDisposables %= (`D.snoc` CD.disposing itemGizmo)
--                 -- Remove the todo from the model
--                 items %= M.delete k
--                 -- on re-render the todo Shim will not get rendered and will be removed by react
--                 lift doRender
--             maybe (pure mempty) pure ret

--         MakeItemAction keyMaker itemModelMaker -> do
--             n <- keyMaker <$> use idx
--             idx .= n
--             pure $ D.singleton $ MakerCommand $ C.Maker.MakerCommand $ do
--                 sm <- R.hoistWithAction (ListAction . ItemAction n) (
--                     itemModelMaker n >>= mkItemGizmo)
--                 pure . ListAction $ AddItemAction n sm

--         AddItemAction n v -> do
--             items %= M.insert n v
--             doRender

--         ItemAction k _ -> fmap (ListCommand . ItemCommand k) <$>
--             (magnify (_ItemAction . to snd)
--             (zoom (items . at k . _Just) itemGadget))

--         SetFilterAction ftr -> do
--             itemsFilter .= ftr
--             doRender
--   where
--       doRender = fmap RenderCommand <$> G.withGadgetT G.Render.RenderAction G.Render.gadget
