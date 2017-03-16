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
    , mkSuperModel
    , Widget
    , Design
    , Replica
    , SuperModel
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
import qualified Glazier.React.Widget as R
import qualified JavaScript.Extras as JE

data Command key itemWidget
    = RenderCommand (R.SuperModel (Model key itemWidget) Plan) [JE.Property] J.JSVal
    | DisposeCommand CD.SomeDisposable
    | MakerCommand (F (R.Maker (Action key itemWidget)) (Action key itemWidget))
    | ItemCommand key (R.WidgetCommand itemWidget)

data Action key itemWidget
    = ComponentRefAction J.JSVal
    | RenderAction
    | ComponentDidUpdateAction
    | DestroyItemAction key
    | MakeItemAction (key -> key) (key -> F (R.Maker (R.WidgetAction itemWidget)) (R.WidgetModel itemWidget))
    | AddItemAction key (R.WidgetSuperModel itemWidget)
    | ItemAction key (R.WidgetAction itemWidget)
    | SetFilterAction (R.WidgetSuperModel itemWidget -> Bool)

data Model key itemWidget = Model
    { _uid :: J.JSString
    , _componentRef :: J.JSVal
    , _frameNum :: Int
    , _deferredCommands :: D.DList (Command key itemWidget)
    , _className ::J.JSString
    , _itemKey :: key
    , _itemsModel :: M.Map key (R.WidgetSuperModel itemWidget)
    , _itemsFilter :: R.WidgetSuperModel itemWidget -> Bool
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
    :: G.WindowT (R.WidgetDesign itemWidget) (R.ReactMlT Identity) ()
    -> R.ReactMlT Identity ()
    -> Replica key itemWidget
    -> F (R.Maker (Action key itemWidget)) Plan
mkPlan itemWindow separator mm = Plan
    <$> R.getComponent
    <*> (R.mkRenderer mm $ const (render itemWindow separator))
    <*> (R.mkHandler $ pure . pure . ComponentRefAction)
    <*> (R.mkHandler $ pure . pure . const ComponentDidUpdateAction)

instance ( CD.Disposing (R.WidgetPlan itemWidget)
         , CD.Disposing (R.WidgetModel itemWidget)
         ) =>
         CD.Disposing (Model key itemWidget) where
    disposing s =
        CD.DisposeList $ foldr ((:) . CD.disposing) [] (s ^. itemsModel)

mkSuperModel
    :: G.WindowT (R.WidgetDesign itemWidget) (R.ReactMlT Identity) ()
    -> R.ReactMlT Identity ()
    -> Model key itemWidget
    -> F (R.Maker (Action key itemWidget)) (SuperModel key itemWidget)
mkSuperModel itemWindow separator mdl = R.mkSuperModel (mkPlan itemWindow separator) (R.Design mdl)

data Widget key itemWidget
instance R.IsWidget (Widget key itemWidget) where
    type WidgetAction (Widget key itemWidget) = Action key itemWidget
    type WidgetCommand (Widget key itemWidget) = Command key itemWidget
    type WidgetModel (Widget key itemWidget) = Model key itemWidget
    type WidgetPlan (Widget key itemWidget) = Plan
type Design key itemWidget = R.WidgetDesign (Widget key itemWidget)
type Replica key itemWidget = R.WidgetReplica (Widget key itemWidget)
type SuperModel key itemWidget = R.WidgetSuperModel (Widget key itemWidget)
instance CD.Disposing Plan
instance HasPlan (R.Design (Model key itemWidget) Plan) where
    plan = R.widgetPlan
instance HasModel (R.Design (Model key itemWidget) Plan) key itemWidget where
    model = R.widgetModel
instance HasPlan (R.SuperModel (Model key itemWidget) Plan) where
    plan = R.design . plan
instance HasModel (R.SuperModel (Model key itemWidget) Plan) key itemWidget where
    model = R.design . model

-- | This is used by parent components to render this component
window :: Monad m => G.WindowT (Design key itemWidget) (R.ReactMlT m) ()
window = do
    s <- ask
    lift $ R.lf (s ^. component . to JE.toJS)
        [ ("key",  s ^. uid . to JE.toJS)
        , ("render", s ^. onRender . to JE.toJS)
        , ("ref", s ^. onComponentRef . to JE.toJS)
        , ("componentDidUpdate", s ^. onComponentDidUpdate . to JE.toJS)
        ]

-- | This is used by the React render callback
render
    :: Monad m
    => G.WindowT (R.WidgetDesign itemWidget) (R.ReactMlT m) ()
    -> R.ReactMlT m ()
    -> G.WindowT (Design key itemWidget) (R.ReactMlT m) ()
render itemWindow separator = do
    s <- ask
    items <- fmap (view R.design) . filter (s ^. itemsFilter) . fmap snd .  M.toList <$> view itemsModel
    lift $ R.bh (JE.strJS "ul") [ ("key", s ^. uid . to JE.toJS)
                                 , ("className", s ^. className . to JE.toJS)
                                 ] $ do
        let itemsWindows = (view G._WindowT itemWindow) <$> items
            separatedWindows = DL.intersperse separator itemsWindows
        sequenceA_ separatedWindows

gadget
    :: (Ord key, Monad m, CD.Disposing (R.WidgetModel itemWidget), CD.Disposing (R.WidgetPlan itemWidget))
    => (R.WidgetModel itemWidget -> F (R.Maker (R.WidgetAction itemWidget)) (R.WidgetSuperModel itemWidget))
    -> G.GadgetT (R.WidgetAction itemWidget) (R.WidgetSuperModel itemWidget) m (D.DList (R.WidgetCommand itemWidget))
    -> G.GadgetT (Action key itemWidget) (SuperModel key itemWidget) m (D.DList (Command key itemWidget))
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
