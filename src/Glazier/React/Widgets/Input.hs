{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Widgets.Input
    ( Action(..)
    , AsAction(..)
    , Schema(..)
    , HasSchema(..)
    , Plan(..)
    , HasPlan(..)
    , Outline
    , Model
    , Widget
    , widget
    , whenKeyDown
    , whenBlur
    ) where

import Control.Applicative
import qualified Control.Disposable as CD
import Control.Lens
import Control.Monad.Free.Church
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import qualified Data.DList as D
import qualified Data.JSString as J
import qualified GHC.Generics as G
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import qualified Glazier as G
import qualified Glazier.React as R
import qualified Glazier.React.Gadgets.Property as G.Property
import qualified Glazier.React.Windows.Component as WComponent
import qualified JavaScript.Extras as JE

type Command = G.Property.Command

data Action
    = SubmitAction J.JSVal J.JSString
    | CancelAction J.JSVal
    | BlurAction J.JSVal
    | ChangedAction J.JSVal J.JSString
    | ResetAction J.JSVal

data Schema = Schema
    { _className :: J.JSString
    , _placeholder :: J.JSString
    }

type Model = Schema
type Outline = Schema
instance R.ToOutline Model Outline where outline = id

mkModel :: Outline -> F (R.Maker Action) Model
mkModel = pure

data Plan = Plan
    { _componentPlan :: WComponent.Plan
    , _onKeyDown :: J.Callback (J.JSVal -> IO ())
    , _onBlur :: J.Callback (J.JSVal -> IO ())
    , _onChanged :: J.Callback (J.JSVal -> IO ())
    } deriving (G.Generic)

makeClassyPrisms ''Action
makeClassy ''Plan
makeClassy ''Schema

mkPlan :: R.Frame Model Plan -> F (R.Maker Action) Plan
mkPlan frm = Plan
    <$> (WComponent.mkPlan render frm)
    <*> (R.mkHandler onKeyDown')
    <*> (R.mkHandler onBlur')
    <*> (R.mkHandler onChanged')

instance CD.Disposing Plan
instance CD.Disposing Model where
    disposing _ = CD.DisposeNone

-- Link Glazier.React.Model's HasPlan/HasModel with this widget's HasPlan/HasModel from makeClassy
instance HasPlan (R.Scene Model Plan) where
    plan = R.plan
instance HasSchema (R.Scene Model Plan) where
    schema = R.model
instance HasPlan (R.Gizmo Model Plan) where
    plan = R.scene . plan
instance HasSchema (R.Gizmo Model Plan) where
    schema = R.scene . schema


-- link the HasPlan for the composites
instance WComponent.HasPlan (R.Scene Model Plan) where
    plan = R.plan . componentPlan
instance WComponent.HasPlan (R.Gizmo Model Plan) where
    plan = R.scene . WComponent.plan

type Widget = R.Widget Action Outline Model Plan Command
widget :: Widget
widget = R.Widget
    mkModel
    mkPlan
    window
    gadget

-- | Exposed to parent components to render this component
window :: G.WindowT (R.Scene Model Plan) R.ReactMl ()
window = WComponent.window []

-- | Internal rendering used by the React render callback
render :: G.WindowT (R.Scene Model Plan) R.ReactMl ()
render = do
    s <- ask
    lift $ R.lf "input"
        [ ("key", s ^. WComponent.key . to JE.toJS')
        , ("className", s ^. className . to JE.toJS')
        , ("placeholder", s ^. placeholder . to JE.toJS')
        , ("autoFocus", JE.toJS' True)
        , ("onKeyDown", s ^. onKeyDown . to JE.toJS')
        , ("onBlur", s ^. onBlur . to JE.toJS')
        , ("onChanged", s ^. onChanged . to JE.toJS')
        ]

whenKeyDown :: J.JSVal -> MaybeT IO (J.JSVal, Maybe J.JSString)
whenKeyDown evt = do
        sevt <- MaybeT $ pure $ JE.fromJS evt
        kevt <- MaybeT $ pure $ R.parseKeyboardEvent sevt
        let evt' = R.parseEvent sevt
            k = R.keyCode kevt
        input <- lift $ pure . JE.toJS . R.target $ evt'
        case k of
            -- FIXME: ESCAPE_KEY
            27 -> pure (input, Nothing)
            -- FIXME: ENTER_KEY
            13 -> do
                v <- MaybeT $ JE.fromJS' <$> JE.getProperty "value" input
                pure (input, Just v)
            _ -> empty

onKeyDown' :: J.JSVal -> MaybeT IO [Action]
onKeyDown' = R.eventHandlerM whenKeyDown goLazy
  where
    goLazy :: (J.JSVal, Maybe J.JSString) -> MaybeT IO [Action]
    goLazy (j, ms) = pure $
        maybe [CancelAction j] (pure . SubmitAction j) ms

whenBlur :: J.JSVal -> MaybeT IO J.JSVal
whenBlur evt = do
        sevt <- MaybeT $ pure $ JE.fromJS evt
        let evt' = R.parseEvent sevt
        lift $ pure . JE.toJS . R.target $ evt'

onBlur' :: J.JSVal -> MaybeT IO [Action]
onBlur' = R.eventHandlerM whenBlur goLazy
  where
    goLazy :: J.JSVal -> MaybeT IO [Action]
    goLazy j = pure [BlurAction j]


whenChanged :: J.JSVal -> MaybeT IO (J.JSVal, J.JSString)
whenChanged evt = do
        sevt <- MaybeT $ pure $ JE.fromJS evt
        let evt' = R.parseEvent sevt
        input <- lift $ pure . JE.toJS . R.target $ evt'
        v <- MaybeT $ JE.fromJS' <$> JE.getProperty "value" input
        pure (input, v)

onChanged' :: J.JSVal -> MaybeT IO [Action]
onChanged' = R.eventHandlerM whenChanged goLazy
  where
    goLazy :: (J.JSVal, J.JSString) -> MaybeT IO [Action]
    goLazy (j, s) = pure [ChangedAction j s]

gadget :: G.Gadget Action s (D.DList Command)
gadget = do
    a <- ask
    case a of
        ResetAction j -> pure $ D.singleton $ G.Property.SetPropertyCommand j ("value", JE.toJS' J.empty)
        _ -> pure mempty
