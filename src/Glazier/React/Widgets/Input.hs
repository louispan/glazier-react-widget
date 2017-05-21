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
import Control.Concurrent.MVar
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
    , _autoFocus :: Bool
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

mkPlan
    :: R.HasScene scn Model Plan
    => (scn -> R.RenderProps)
    -> MVar scn
    -> F (R.Maker Action) Plan
mkPlan renderProps frm = Plan
    <$> (WComponent.mkPlan (render renderProps) frm)
    <*> (R.mkHandler onKeyDown')
    <*> (R.mkHandler onBlur')
    <*> (R.mkHandler onChanged')

instance CD.Disposing Plan
instance CD.Disposing Model where
    disposing _ = CD.DisposeNone

-- Link Glazier.React.Model's HasPlan/HasModel with this widget's HasPlan/HasModel from makeClassy
instance HasPlan pln => HasPlan (R.Scene mdl pln) where
    plan = R.plan . plan
instance HasPlan pln => HasPlan (R.Gizmo mdl pln) where
    plan = R.plan . plan
instance HasSchema mdl => HasSchema (R.Scene mdl pln) where
    schema = R.model . schema
instance HasSchema mdl => HasSchema (R.Gizmo mdl pln) where
    schema = R.model . schema

-- link the HasPlan for the composites
instance WComponent.HasPlan Plan where
    plan = componentPlan

type Widget = R.Widget Action Outline Model Plan Command

widget
    :: (forall scn. R.HasScene scn Model Plan => scn -> R.WindowProps)
    -> (forall scn. R.HasScene scn Model Plan => scn -> R.RenderProps)
    -> Widget
widget windowProps renderProps = R.Widget
    mkModel
    (mkPlan renderProps)
    (window windowProps)
    gadget

-- | Exposed to parent components to render this component
window :: R.HasScene scn Model Plan => (scn -> R.WindowProps) -> G.WindowT scn R.ReactMl ()
window = WComponent.window

-- | Internal rendering used by the React render callback
render :: R.HasScene scn Model Plan => (scn -> R.RenderProps) -> G.WindowT scn R.ReactMl ()
render renderProps = do
    s <- ask
    let R.RenderProps (props, hdls) = renderProps s
    lift $
        R.lf
            "input"
            ([ ("key", s ^. R.scene . WComponent.key . to JE.toJS')
             , ("className", s ^. R.scene . className . to JE.toJS')
             , ("placeholder", s ^. R.scene . placeholder . to JE.toJS')
             , ("autoFocus", s ^. R.scene . autoFocus . to JE.toJS')
             ] ++
             props)
            ([ ("onKeyDown", s ^. R.scene . onKeyDown)
             , ("onBlur", s ^. R.scene . onBlur)
             , ("onChanged", s ^. R.scene . onChanged)
             ] ++
             hdls)

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
