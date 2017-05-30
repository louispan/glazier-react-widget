{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

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

-- | List specific command
data Command' k w = ItemCommand k (R.CommandOf w)

-- | Combined Command
data Command k w mdl
    = RenderCommand (D.Render.Command (R.Shared mdl))
    | DisposeCommand D.Dispose.Command
    | MakerCommand (C.Maker.Command (Action k w))
    | ListCommand (Command' k w)

-- | List specific actions
data Action' k w
    = DestroyItemAction k
    | MakeItemAction (k -> k) (k -> F (R.Maker (R.ActionOf w)) (R.ModelOf w))
    | AddItemAction k (R.EntityOf w)
    | ItemAction k (R.ActionOf w)
    | SetFilterAction (R.OutlineOf w -> Bool)

-- | Combined Action
data Action k w
    = RenderAction D.Render.Action
    | DisposeAction D.Dispose.Action
    | ListAction (Action' k w)

data Schema k w (p :: R.Part) = Schema
    { _className :: J.JSString
    , _idx :: k
    , _items :: M.Map k (R.Widget's p w)
    , _itemsFilter :: R.OutlineOf w -> Bool
    }

type Detail k w = Schema k w 'R.BaseEntity'
type Outline k w = Schema k w 'R.Outline'

outline
    :: ( R.ModelOf w ~ R.BaseModelOf w
       , R.HasDetail w
       , R.ToOutline w
       )
    => w -> Detail k w -> Outline k w
outline w (Schema a b c d) =
        Schema a b (view (R.ival . R.detail w . to (R.outline w)) <$> c) d

mkDetail
    :: (R.IsWidget w, R.ModelOf w ~ R.BaseModelOf w)
    => w
    -> Outline k w
    -> F (R.Maker (Action k w)) (Detail k w)
mkDetail w (Schema a b c d) = Schema
    <$> pure a
    <*> pure b
    <*> M.traverseWithKey (\k i -> R.hoistWithAction (ListAction . ItemAction k) (R.mkBaseEntity' w i)) c
    <*> pure d

data Plan = Plan
    { _componentPlan :: D.Component.Plan
    , _renderPlan :: D.Render.Plan
    , _disposePlan :: D.Dispose.Plan
    } deriving (G.Generic)

makeClassyPrisms ''Action
makeClassyPrisms ''Action'
makeClassy ''Schema
makeClassy ''Plan

mkRenderingPlan
    :: D.Component.Display (Action k w) mdl
    -> D.Render.Device mdl
    -> D.Dispose.Device mdl
    -> MVar mdl
    -> F (R.Maker (Action k w)) Plan
mkRenderingPlan component' render' dispose' frm = Plan
    <$> (R.mkRenderingPlan component' frm)
    <*> (R.hoistWithAction RenderAction $ R.mkPlan render')
    <*> (R.hoistWithAction DisposeAction $ R.mkPlan dispose')

instance CD.Disposing Plan

instance (R.ModelOf w ~ R.BaseModelOf w, CD.Disposing (R.EntityOf w)) =>
         CD.Disposing (Detail k w) where
    disposing s = CD.DisposeList $ foldr ((:) . CD.disposing) [] (s ^. items)

instance D.Component.HasPlan Plan where
    plan = componentPlan

instance D.Render.HasPlan Plan where
    plan = renderPlan

instance D.Dispose.HasPlan Plan where
    plan = disposePlan

-- type Widget k w = R.Widget (Action k w) (Outline k w) (Model k w) Plan (Command k w)
-- widget
--     :: (R.IsWidget w, Ord k)
--     => R.ReactMl ()
--     -> w
--     -> (forall scn. R.HasScene scn (Model k w) Plan => scn -> R.WindowProps)
--     -> (forall scn. R.HasScene scn (Model k w) Plan => scn -> R.RenderProps)
--     -> Widget k w
-- widget separator w windowProps renderProps = R.Widget
--     (mkModel w)
--     (mkPlan separator (R.window w) renderProps)
--     (window windowProps)
--     (gadget (R.mkGizmo w) (R.gadget w))

window :: R.Display (Action k w) D.Component.Plan mdl -> G.WindowT mdl R.ReactMl ()
window component' = R.window component'

-- | Internal rendering used by the React render callback
-- render
--     :: (R.HasScene scn (Model k w) Plan, R.IsWidget w)
--     => R.ReactMlT Identity ()
--     -> G.WindowT (R.SceneOf w) R.ReactMl ()
--     -> (scn -> R.RenderProps)
--     -> G.WindowT scn R.ReactMl ()
render
    :: (R.ModelOf w ~ R.BaseModelOf w, R.IsWidget w) => R.ReactMlT Identity ()
    -> w
    -> Lens' mdl (Detail k w)
    -> Lens' mdl Plan
    -> (mdl -> R.RenderAttributes)
    -> G.WindowT mdl R.ReactMl ()
render separator w dtl pln ra = do
    s <- ask
    let R.RenderAttributes (props, hdls) = ra s
    xs <-
        filter ((s ^. dtl . itemsFilter) . R.toOutline' w) .
        fmap (view R.ival . snd) . -- get the item BaseModel
        M.toList <$>
        view (dtl . items) -- get the items
    lift $
        R.bh
            "ul"
            ([ ("key", s ^. pln . D.Component.key . to JE.toJS')
            , ("className", s ^. dtl . className . to JE.toJS')
            ] ++ props)
            hdls $ do
        let itemsWindows = view G._WindowT itemWindow <$> xs
            separatedWindows = DL.intersperse separator itemsWindows
        sequenceA_ separatedWindows
  where
    itemWindow = R.window w
-- gadget
--     :: (Ord k, R.IsWidget w)
--     => (R.ModelOf w -> F (R.Maker (R.ActionOf w)) (R.GizmoOf w))
--     -> G.Gadget (R.ActionOf w) (R.GizmoOf w) (D.DList (R.CommandOf w))
--     -> G.Gadget (Action k w) (R.Gizmo (Model k w) Plan) (D.DList (Command k w))
-- gadget mkItemGizmo itemGadget =
--         (fmap RenderCommand <$> magnify _RenderAction G.Render.gadget)
--     <|> (fmap DisposeCommand <$> magnify _DisposeAction G.Dispose.gadget)
--     <|> (magnify _ListAction (listGadget mkItemGizmo itemGadget))

-- listGadget
--     :: (Ord k, R.IsWidget w)
--     => (R.ModelOf w -> F (R.Maker (R.ActionOf w)) (R.GizmoOf w))
--     -> G.Gadget (R.ActionOf w) (R.GizmoOf w) (D.DList (R.CommandOf w))
--     -> G.Gadget (Action' k w) (R.Gizmo (Model k w) Plan) (D.DList (Command k w))
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
