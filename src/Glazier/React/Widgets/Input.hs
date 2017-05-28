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
    , Detail
    -- , Widget
    -- , widget
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
instance R.ToOutline Detail Outline where outline = id

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

componentDisplay
    :: Lens' mdl Detail
    -> Lens' mdl Plan
    -> (mdl -> R.WindowAttrs)
    -> (mdl -> R.RenderAttrs)
    -> R.Display a D.Component.Plan mdl
componentDisplay dtl pln wa ra = D.Component.display (pln . componentPlan) (const $ render dtl pln ra) wa

mkPlan
    :: R.Display Action D.Component.Plan mdl
    -> MVar mdl
    -> F (R.Maker Action) Plan
mkPlan componentDisplay' frm = Plan
    <$> (R.mkPlan componentDisplay' frm)
    <*> (R.mkHandler onKeyDown')
    <*> (R.mkHandler onBlur')
    <*> (R.mkHandler onChanged')

instance CD.Disposing Plan
instance CD.Disposing Detail where
    disposing _ = CD.DisposeNone

instance D.Component.HasPlan Plan where
    plan = componentPlan

-- | Exposed to parent components to render this component
window :: R.Display Action D.Component.Plan mdl
    -> J.JSVal
    -> G.WindowT mdl R.ReactMl ()
window componentDisplay' = R.window componentDisplay'

-- | Internal rendering used by the React render callback
render :: Lens' mdl Detail -> Lens' mdl Plan -> (mdl -> R.RenderAttrs) -> G.WindowT mdl R.ReactMl ()
render dtl pln ra = do
    s <- ask
    let R.RenderAttrs (props, hdls) = ra s
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

gadget :: G.Gadget Action (R.Shared mdl) (D.DList Command)
gadget = do
    a <- ask
    case a of
        ResetAction j -> pure $ D.singleton $ G.Property.SetPropertyCommand j ("value", JE.toJS' J.empty)
        _ -> pure mempty

-- type Widget = R.Widget Action Outline Detail Plan Command

-- widget
--     :: (forall mdl. R.HasModel mdl Detail Plan => mdl -> R.WindowProps)
--     -> (forall mdl. R.HasModel mdl Detail Plan => mdl -> R.RenderProps)
--     -> Widget
-- widget windowProps renderProps = R.Widget
--     mkModel
--     (mkPlan renderProps)
--     (window windowProps)
--     gadget
