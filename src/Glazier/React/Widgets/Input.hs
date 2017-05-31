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
import qualified Glazier.React.Displays.Component as D.Component
import qualified Glazier.React.Commands.Property as C.Property
import qualified JavaScript.Extras as JE

type Command = C.Property.Command

data Action
    = SubmitAction R.DOMEventTarget J.JSString
    | CancelAction R.DOMEventTarget
    | ChangedAction R.DOMEventTarget J.JSString
    | ResetAction R.DOMEventTarget

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
    , _onChanged :: J.Callback (J.JSVal -> IO ())
    } deriving (G.Generic)

makeClassyPrisms ''Action
makeClassy ''Plan
makeClassy ''Schema

mkComponentPlan
    :: R.Display Action D.Component.Plan mdl
    -> MVar mdl
    -> F (R.Maker Action) Plan
mkComponentPlan component' frm = Plan
    <$> (R.mkComponentPlan component' frm)
    <*> (R.mkHandler onKeyDown')
    <*> (R.mkHandler onChanged')

instance CD.Disposing Plan
instance CD.Disposing Detail where
    disposing _ = CD.DisposeNone

instance D.Component.HasPlan Plan where
    plan = componentPlan

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
             , ("onChanged", s ^. pln . onChanged)
             ] ++
             hdls)

whenKeyDown :: J.JSVal -> MaybeT IO (R.DOMEventTarget, Maybe J.JSString)
whenKeyDown evt = do
        syn <- MaybeT $ pure $ JE.fromJS evt
        kevt <- MaybeT $ pure $ R.parseKeyboardEvent syn
        let evt' = R.parseEvent syn
            k = R.key kevt
        target <- lift $ pure . R.target $ evt'
        case k of
            "Escape" -> pure (target, Nothing)
            "Enter" -> do
                v <- MaybeT $ JE.fromJS' <$> JE.getProperty "value" (JE.toJS target)
                pure (target, Just v)
            _ -> empty

onKeyDown' :: J.JSVal -> MaybeT IO [Action]
onKeyDown' = R.eventHandlerM whenKeyDown lazily
  where
    lazily :: (R.DOMEventTarget, Maybe J.JSString) -> MaybeT IO [Action]
    lazily (j, ms) = pure $
        maybe [CancelAction j] (pure . SubmitAction j) ms

onChanged' :: J.JSVal -> MaybeT IO [Action]
onChanged' = R.eventHandlerM strictly lazily
  where
    strictly evt = do
        target <- MaybeT . pure $ (JE.fromJS evt) <&> (R.target . R.parseEvent)
        v <- MaybeT $ JE.fromJS' <$> JE.getProperty "value" (JE.toJS target)
        pure (target, v)
    lazily :: (R.DOMEventTarget, J.JSString) -> MaybeT IO [Action]
    lazily (j, s) = pure [ChangedAction j s]

gadget :: G.Gadget Action (R.Shared mdl) (D.DList Command)
gadget = do
    a <- ask
    case a of
        ResetAction j -> pure $ D.singleton $ C.Property.SetPropertyCommand (JE.toJS j) ("value", JE.toJS' J.empty)
        _ -> pure mempty

type Widget mdl = R.Widget Action Outline Detail Plan Command mdl

widget
    :: Lens' mdl Detail
    -> Lens' mdl Plan
    -> (mdl -> R.RenderAttributes)
    -> Widget mdl
widget dtl pln ra = R.Widget
    dtl
    pln
    outline
    mkDetail
    (mkComponentPlan component')
    (R.window component')
    gadget
  where
    component' = D.Component.display (pln . componentPlan) (render dtl pln ra) mempty
