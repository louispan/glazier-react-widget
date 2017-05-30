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
    ( Command
    , Action(..)
    , AsAction(..)
    , Schema(..)
    , HasSchema(..)
    , Plan(..)
    , HasPlan(..)
    , Outline
    , Detail
    , Widget
    , widget
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
import qualified Glazier.React.Commands.Property as G.Property
import qualified Glazier.React.Displays.Component as D.Component
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

type Detail = Schema
type Outline = Schema

outline :: Detail -> Outline
outline = id

mkDetail :: Outline -> F (R.Maker Action) Detail
mkDetail = pure

data Plan = Plan
    { _componentPlan :: D.Component.Plan
    , _onKeyDown :: J.Callback (J.JSVal -> IO ())
    , _onBlur :: J.Callback (J.JSVal -> IO ())
    , _onChanged :: J.Callback (J.JSVal -> IO ())
    } deriving (G.Generic)

makeClassyPrisms ''Action
makeClassy ''Plan
makeClassy ''Schema

mkRenderingPlan
    :: R.Display Action D.Component.Plan mdl
    -> MVar mdl
    -> F (R.Maker Action) Plan
mkRenderingPlan component' frm = Plan
    <$> (R.mkRenderingPlan component' frm)
    <*> (R.mkHandler onKeyDown')
    <*> (R.mkHandler onBlur')
    <*> (R.mkHandler onChanged')

instance CD.Disposing Plan
instance CD.Disposing Detail where
    disposing _ = CD.DisposeNone

instance D.Component.HasPlan Plan where
    plan = componentPlan

-- | Internal rendering used by the React render callback
render :: Lens' mdl Detail -> Lens' mdl Plan -> (mdl -> R.RenderAttributes) -> G.WindowT mdl R.ReactMl ()
render dtl pln ra = do
    s <- ask
    let R.RenderAttributes (props, hdls) = ra s
    lift $
        R.lf
            "input"
            ([ ("key", s ^. pln . D.Component.key . to JE.toJS')
             , ("className", s ^. dtl . className . to JE.toJS')
             , ("placeholder", s ^. dtl . placeholder . to JE.toJS')
             , ("autoFocus", s ^. dtl . autoFocus . to JE.toJS')
             ] ++
             props)
            ([ ("onKeyDown", s ^. pln . onKeyDown)
             , ("onBlur", s ^. pln . onBlur)
             , ("onChanged", s ^. pln . onChanged)
             ] ++
             hdls)

whenKeyDown :: J.JSVal -> MaybeT IO (J.JSVal, Maybe J.JSString)
whenKeyDown evt = do
        syn <- MaybeT $ pure $ JE.fromJS evt
        kevt <- MaybeT $ pure $ R.parseKeyboardEvent syn
        let evt' = R.parseEvent syn
            k = R.keyCode kevt
        target <- lift $ pure . JE.toJS . R.target $ evt'
        case k of
            -- FIXME: ESCAPE_KEY
            27 -> pure (target, Nothing)
            -- FIXME: ENTER_KEY
            13 -> do
                v <- MaybeT $ JE.fromJS' <$> JE.getProperty "value" target
                pure (target, Just v)
            _ -> empty

onKeyDown' :: J.JSVal -> MaybeT IO [Action]
onKeyDown' = R.eventHandlerM whenKeyDown goLazy
  where
    goLazy :: (J.JSVal, Maybe J.JSString) -> MaybeT IO [Action]
    goLazy (j, ms) = pure $
        maybe [CancelAction j] (pure . SubmitAction j) ms

onBlur' :: J.JSVal -> MaybeT IO [Action]
onBlur' = R.eventHandlerM goStrict goLazy
  where
    goStrict evt = MaybeT . pure $ (JE.fromJS evt) <&> (R.target . R.parseEvent)
    goLazy :: R.DOMEventTarget -> MaybeT IO [Action]
    goLazy j = pure [BlurAction (JE.toJS j)]


onChanged' :: J.JSVal -> MaybeT IO [Action]
onChanged' = R.eventHandlerM goStrict goLazy
  where
    goStrict evt = do
        target <- MaybeT . pure $ (JE.fromJS evt) <&> (JE.toJS . R.target . R.parseEvent)
        v <- MaybeT $ JE.fromJS' <$> JE.getProperty "value" target
        pure (target, v)
    goLazy :: (J.JSVal, J.JSString) -> MaybeT IO [Action]
    goLazy (j, s) = pure [ChangedAction j s]

gadget :: G.Gadget Action (R.Shared mdl) (D.DList Command)
gadget = do
    a <- ask
    case a of
        ResetAction j -> pure $ D.singleton $ G.Property.SetPropertyCommand j ("value", JE.toJS' J.empty)
        _ -> pure mempty

type Widget mdl = R.Widget Action Outline Detail Plan Command mdl

widget
    :: Lens' mdl Detail
    -> Lens' mdl Plan
    -> (mdl -> R.WindowAttributes)
    -> (mdl -> R.RenderAttributes)
    -> Widget mdl
widget dtl pln wa ra = R.Widget
    outline
    dtl
    pln
    mkDetail
    (mkRenderingPlan component')
    (R.window component')
    gadget
  where
    component' = D.Component.display (pln . componentPlan) (render dtl pln ra) wa
