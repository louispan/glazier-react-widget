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

data Command key itemWidget
    = RenderCommand (R.SuperModel (Model key itemWidget) Plan) [JE.Property] J.JSVal
    | DisposeCommand CD.SomeDisposable
    | MakerCommand (F (R.Maker (Action key itemWidget)) (Action key itemWidget))
    | ItemCommand key (R.CommandOf itemWidget)

data Action key itemWidget
    = ComponentRefAction J.JSVal
    | RenderAction
    | ComponentDidUpdateAction
    | DestroyItemAction key
    | MakeItemAction (key -> key) (key -> F (R.Maker (R.ActionOf itemWidget)) (R.ModelOf itemWidget))
    | AddItemAction key (R.SuperModelOf itemWidget)
    | ItemAction key (R.ActionOf itemWidget)
    | SetFilterAction (R.SuperModelOf itemWidget -> Bool)

data Model key itemWidget = Model
    { _uid :: J.JSString
    , _componentRef :: J.JSVal
    , _frameNum :: Int
    , _deferredCommands :: D.DList (Command key itemWidget)
    , _className ::J.JSString
    , _itemKey :: key
    , _itemsModel :: M.Map key (R.SuperModelOf itemWidget)
    , _itemsFilter :: R.SuperModelOf itemWidget -> Bool
    }

data Plan = Plan
    { _component :: R.ReactComponent
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
    -> R.Frame (Model key itemWidget) Plan
    -> F (R.Maker (Action key itemWidget)) Plan
mkPlan separator itemWindow frm = Plan
    <$> R.getComponent
    <*> (R.mkRenderer frm $ const (render separator itemWindow))
    <*> (R.mkHandler $ pure . pure . ComponentRefAction)
    <*> (R.mkHandler $ pure . pure . const ComponentDidUpdateAction)

instance CD.Disposing Plan
instance (CD.Disposing (R.SuperModelOf itemWidget)) =>
         CD.Disposing (Model key itemWidget) where
    disposing s = CD.DisposeList $ foldr ((:) . CD.disposing) [] (_itemsModel s)

-- Link Glazier.React.Model's HasPlan/HasModel with this widget's HasPlan/HasModel from makeClassy
instance HasPlan (R.Design (Model key itemWidget) Plan) where
    plan = R.plan
instance HasModel (R.Design (Model key itemWidget) Plan) key itemWidget where
    model = R.model
instance HasPlan (R.SuperModel (Model key itemWidget) Plan) where
    plan = R.design . plan
instance HasModel (R.SuperModel (Model key itemWidget) Plan) key itemWidget where
    model = R.design . model

type Design key itemWidget = R.Design (Model key itemWidget) Plan
type Frame key itemWidget = R.Frame (Model key itemWidget) Plan
type SuperModel key itemWidget = R.SuperModel (Model key itemWidget) Plan

type Widget key itemWidget = R.Widget (Command key itemWidget) (Action key itemWidget) (Model key itemWidget) Plan
widget
    :: (R.IsWidget itemWidget, Ord key)
    => R.ReactMlT Identity ()
    -> itemWidget
    -> R.Widget (Command key itemWidget) (Action key itemWidget) (Model key itemWidget) Plan
widget separator itemWidget = R.Widget
    (mkPlan separator (R.window itemWidget))
    window
    (gadget (R.mkSuperModel itemWidget) (R.gadget itemWidget))

-- | Exposed to parent components to render this component
window :: Monad m => G.WindowT (R.Design (Model key itemWidget) Plan) (R.ReactMlT m) ()
window = do
    s <- ask
    lift $ R.lf (s ^. component . to JE.toJS)
        [ ("key",  s ^. uid . to JE.toJS)
        , ("render", s ^. onRender . to JE.toJS)
        , ("ref", s ^. onComponentRef . to JE.toJS)
        , ("componentDidUpdate", s ^. onComponentDidUpdate . to JE.toJS)
        ]

-- | Internal rendering used by the React render callback
render
    :: R.ReactMlT Identity ()
    -> G.WindowT (R.DesignOf itemWidget) (R.ReactMlT Identity) ()
    -> G.WindowT (R.Design (Model key itemWidget) Plan) (R.ReactMlT Identity) ()
render separator itemWindow = do
    s <- ask
    items <- fmap (view R.design) . filter (s ^. itemsFilter) . fmap snd .  M.toList <$> view itemsModel
    lift $ R.bh (JE.strJS "ul") [ ("key", s ^. uid . to JE.toJS)
                                 , ("className", s ^. className . to JE.toJS)
                                 ] $ do
        let itemsWindows = (view G._WindowT itemWindow) <$> items
            separatedWindows = DL.intersperse separator itemsWindows
        sequenceA_ separatedWindows

gadget
    :: (Ord key, CD.Disposing (R.ModelOf itemWidget), CD.Disposing (R.PlanOf itemWidget))
    => (R.ModelOf itemWidget -> F (R.Maker (R.ActionOf itemWidget)) (R.SuperModelOf itemWidget))
    -> G.GadgetT (R.ActionOf itemWidget) (R.SuperModelOf itemWidget) Identity (D.DList (R.CommandOf itemWidget))
    -> G.GadgetT (Action key itemWidget) (R.SuperModel (Model key itemWidget) Plan) Identity (D.DList (Command key itemWidget))
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
            cmds <- use deferredCommands
            deferredCommands .= mempty
            pure cmds

        DestroyItemAction k -> do
            -- queue up callbacks to be released after rerendering
            ret <- runMaybeT $ do
                itemSuperModel <- MaybeT $ use (itemsModel . at k)
                let junk = CD.disposing itemSuperModel
                deferredCommands %= (`D.snoc` DisposeCommand junk)
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

        ItemAction key _ -> fmap (ItemCommand key) <$>
            (magnify (_ItemAction . to snd)
            (zoom (itemsModel . at key . _Just) itemGadget))

        SetFilterAction ftr -> do
            itemsFilter .= ftr
            D.singleton <$> (R.basicRenderCmd frameNum componentRef RenderCommand)
