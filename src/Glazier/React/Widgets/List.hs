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

module Glazier.React.Widget.List
    ( TodosKey
    , TodosValue
    , TodosCommand'
    , TodosAction'
    , TodosModel'
    , Command(..)
    , Action(..)
    , AsAction(..)
    , Gasket(..)
    , HasGasket(..)
    , mkGasket
    , Model(..)
    , HasModel(..)
    , GModel
    , MModel
    , SuperModel
    , mkSuperModel
    , window
    , gadget
    ) where

import Control.Concurrent.MVar
import qualified Control.Disposable as CD
import Control.Lens
import Control.Monad.Free.Church
import Control.Monad.Morph
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import qualified Data.DList as D
import Data.Foldable
import qualified Data.JSString as J
import qualified Data.Map.Strict as M
import Data.Semigroup
import qualified GHC.Generics as G
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
import qualified Glazier as G
import qualified Glazier.React.Class as R
import qualified Glazier.React.Component as R
import qualified Glazier.React.Maker as R
import qualified Glazier.React.Markup as R
import qualified Glazier.React.Widget.Input as W.Input
import qualified JavaScript.Extras as JE
-- import qualified Todo.Todo as TD.Todo

-- type TodosKey = Int

-- type TodosValue = TD.Todo.SuperModel

-- type TodosModel' = M.Map TodosKey TodosValue

-- type TodosCommand' = (TodosKey, TD.Todo.Command)

-- type TodosAction' = (TodosKey, TD.Todo.Action)

data Command key itemWidget
    = RenderCommand (SuperModel key itemWidget) [JE.Property] J.JSVal
    | DisposeCommand CD.SomeDisposable
    | MakerCommand (F (R.Maker (Action key itemWidget)) (Action key itemWidget))
    | SendActionsCommand [Action key itemWidget]
    | ItemCommand key (R.Command itemWidget)

data Action key itemWidget
    = ComponentRefAction J.JSVal
    | ComponentDidUpdateAction
    | DestroyItemAction key
    | MakeItemAction (key -> key) (key -> R.Model itemWidget)
    | AddItemAction key (R.SuperModel itemWidget)
    | ItemAction key (R.Action itemWidget)

data Model key itemWidget = Model
    { _uid :: J.JSString
    , _componentRef :: J.JSVal
    , _className ::J.JSString
    , _frameNum :: Int
    , _deferredCommands :: D.DList (Command key itemWidget)
    , _itemKey :: key
    , _itemsModel :: M.Map key (R.SuperModel itemWidget)
    }

data Gasket = Gasket
    { _component :: R.ReactComponent
    , _onRender ::  J.Callback (J.JSVal -> IO J.JSVal)
    , _onComponentRef :: J.Callback (J.JSVal -> IO ())
    , _onComponentDidUpdate :: J.Callback (J.JSVal -> IO ())
    } deriving (G.Generic)

----------------------------------------------------------
-- The following should be the same per widget (except for type params)
-- | Gasket and pure state
type GModel key itemWidget = (Gasket, Model key itemWidget)
-- | Mutable model for rendering callback
type MModel key itemWidget = MVar (GModel key itemWidget)
-- | Contains MModel and GModel
type SuperModel key itemWidget = (MModel key itemWidget, GModel key itemWidget)
makeClassyPrisms ''Action
makeClassy ''Gasket
makeClassy ''Model
instance CD.Disposing Gasket
-- GModel
instance R.HasGModel (GModel key itemWidget) (GModel key itemWidget) where
    gModel = id
instance HasGasket (GModel key itemWidget) where
    gasket = _1
instance HasModel (GModel key itemWidget) key itemWidget where
    model = _2
instance CD.Disposing (R.SuperModel itemWidget) => CD.Disposing (GModel key itemWidget)
-- MModel
instance R.HasMModel (MModel key itemWidget) (GModel key itemWidget) where
    mModel = id
-- SuperModel
instance R.HasMModel (SuperModel key itemWidget) (GModel key itemWidget) where
    mModel = _1
instance R.HasGModel (SuperModel key itemWidget) (GModel key itemWidget) where
    gModel = _2
instance HasGasket (SuperModel key itemWidget) where
    gasket = R.gModel . gasket
instance HasModel (SuperModel key itemWidget) key itemWidget where
    model = R.gModel . model
instance CD.Disposing (R.SuperModel itemWidget) => CD.Disposing (SuperModel key itemWidget) where
    disposing s = CD.disposing $ s ^. R.gModel
data Widget key itemWidget
instance R.Widget (Widget key itemWidget) where
    type Action (Widget key itemWidget) = Action key itemWidget
    type Command (Widget key itemWidget) = Command key itemWidget
    type Model (Widget key itemWidget) = Model key itemWidget
    type Gasket (Widget key itemWidget) = Gasket
-- End same code per widget
----------------------------------------------------------

-- | This might be different per widget
instance CD.Disposing (R.SuperModel itemWidget) => CD.Disposing (Model key itemWidget) where
    disposing s = CD.DisposeList $ foldr ((:) . CD.disposing) [] (s ^. itemsModel)

-- mkSuperModel :: W.Input.Model -> (W.Input.SuperModel -> (Model key itemWidget)) -> F (R.Maker (Action key itemWidget)) (SuperModel key itemWidget)
-- mkSuperModel inputModel f = do
--     inputSuperModel <- hoistF (R.mapAction $ review _InputAction) $
--         W.Input.mkSuperModel $ inputModel
--     R.mkSuperModel mkGasket $ \cbs -> (cbs, f inputSuperModel)

-- End similar code per widget
----------------------------------------------------------

mkGasket :: G.WindowT (R.GModel itemWidget) (R.ReactMlT Identity) () -> MVar (GModel key itemWidget) -> F (R.Maker (Action key itemWidget)) Gasket
mkGasket itemWindow ms = Gasket
    <$> R.getComponent
    <*> (R.mkRenderer ms $ const (render itemWindow))
    <*> (R.mkHandler $ pure . pure . ComponentRefAction)
    <*> (R.mkHandler $ pure . pure . const ComponentDidUpdateAction)

-- | This is used by parent components to render this component
window :: Monad m => G.WindowT (GModel key itemWidget) (R.ReactMlT m) ()
window = do
    s <- ask
    lift $ R.lf (s ^. component . to J.jsval)
        [ ("key",  s ^. uid . to J.jsval)
        , ("render", s ^. onRender . to J.jsval)
        , ("ref", s ^. onComponentRef . to J.jsval)
        , ("componentDidUpdate", s ^. onComponentDidUpdate . to J.jsval)
        ]

-- | This is used by the React render callback
-- FIXME: add separator
render :: Monad m => G.WindowT (R.GModel itemWidget) (R.ReactMlT m) () -> G.WindowT (GModel key itemWidget) (R.ReactMlT m) ()
render itemWindow = do
    s <- ask
    items <- fmap (view R.gModel . snd) . M.toList <$> view itemsModel
    lift $ R.bh (JE.strval "ul") [ ("key", s ^. uid . to J.jsval)
                                 , ("className", s ^. className . to J.jsval)
                                 ] $
        traverse_ (view G._WindowT itemWindow) items

gadget
    :: Monad m
    => (R.Model itemWidget -> F (R.Maker (R.Action itemWidget)) (R.SuperModel itemWidget))
    -> G.GadgetT (R.Action itemWidget) (R.SuperModel itemWidget) m (D.DList (R.Command itemWidget))
    -> G.GadgetT (Action key itemWidget) (SuperModel key itemWidget) m (D.DList (Command key itemWidget))
gadget mkItemSuperModel itemGadget = do
    a <- ask
    case a of
        ComponentRefAction node -> do
            componentRef .= node
            pure mempty

        ComponentDidUpdateAction -> do
            -- Run delayed commands that need to wait until frame is re-rendered
            -- Eg focusing after other rendering changes
            cmds <- use deferredCommands
            deferredCommands .= mempty
            pure cmds

        DestroyItemAction k -> do
            -- queue up callbacks to be released after rerendering
            (maybe (pure mempty) pure) <$> runMaybeT $ do
                (_, itemsModel) <- MaybeT $ preuse (itemsModel . at k)
                let junk = CD.disposing itemsModel
                deferredCommands %= (`D.snoc` DisposeCommand junk)
                -- Remove the todo from the model
                itemsModel %= M.delete k
                -- on re-render the todo Shim will not get rendered and will be removed by react
                lift $ D.singleton <$> renderCmd

        MakeItemAction keyMaker itemModelMaker -> do
            n <- keyMaker <$> use itemKey
            itemKey .= n
            pure $ D.singleton $ MakerCommand $ do
                sm <- hoistF (R.mapAction $ \act -> ItemAction (n, act)) $
                    mkItemSuperModel $ itemModelMaker n
                pure $ AddItemAction n sm

        AddItemAction n v -> do
            itemsModel %= M.insert n v
            D.singleton <$> renderCmd

        ItemAction key act -> do
            (maybe (pure mempty) pure) <$> runMaybeT $ do
                xs <- MaybeT $ preuse (itemsModel . at k)
                -- run the item gadget logic
                (cmds, sm') <- lift $ lift $ view G._GadgetT itemGadget a sm
                -- replace the item state in the map
                lift $ itemsModel %= M.insert k sm'
                -- annotate cmd with the key
                pure $ (ItemCommand k) <$> cmds
