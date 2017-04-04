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
    ( Command(..)
    , Action(..)
    , AsAction(..)
    , Design(..)
    , HasDesign(..)
    , Plan(..)
    , HasPlan(..)
    , mkPlan
    , Model
    , Outline
    , Scene
    , Frame
    , Gizmo
    , Widget
    , widget
    , window
    , gadget
    , whenKeyDown
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
import qualified Glazier.React.Component as R
import qualified Glazier.React.Event as R
import qualified Glazier.React.Maker as R
import qualified Glazier.React.Markup as R
import qualified Glazier.React.Model as R
import qualified Glazier.React.Widget as R
import qualified JavaScript.Extras as JE

data Command
    = SetPropertyCommand JE.Property J.JSVal

data Action
    = SendCommandsAction [Command]
    | SubmitAction J.JSString
    | InputRefAction J.JSVal

type Model = Design
type Outline = Design

data Design = Design
    { _placeholder :: J.JSString
    , _className :: J.JSString
    }

data Plan = Plan
    { _component :: R.ReactComponent
    , _key :: J.JSString
    , _inputRef :: J.JSVal
    , _onRender :: J.Callback (J.JSVal -> IO J.JSVal)
    , _onInputRef :: J.Callback (J.JSVal -> IO ())
    , _onKeyDown :: J.Callback (J.JSVal -> IO ())
    } deriving (G.Generic)

makeClassyPrisms ''Action
makeClassy ''Plan
makeClassy ''Design

mkPlan :: R.Frame Model Plan -> F (R.Maker Action) Plan
mkPlan frm = Plan
    <$> R.getComponent
    <*> R.mkKey
    <*> pure J.nullRef
    <*> (R.mkRenderer frm $ const render)
    <*> (R.mkHandler $ pure . pure . InputRefAction)
    <*> (R.mkHandler onKeyDown')

instance CD.Disposing Plan
instance CD.Disposing Model where
    disposing _ = CD.DisposeNone

-- Link Glazier.React.Model's HasPlan/HasModel with this widget's HasPlan/HasModel from makeClassy
instance HasPlan (R.Scene Model Plan) where
    plan = R.plan
instance HasDesign (R.Scene Model Plan) where
    design = R.model
instance HasPlan (R.Gizmo Model Plan) where
    plan = R.scene . plan
instance HasDesign (R.Gizmo Model Plan) where
    design = R.scene . design

type Scene = R.Scene Model Plan
type Frame = R.Frame Model Plan
type Gizmo = R.Gizmo Model Plan

type Widget = R.Widget Command Action Outline Model Plan
widget :: Widget
widget = R.Widget
    mkPlan
    window
    gadget

-- | Exposed to parent components to render this component
window :: G.WindowT (R.Scene Model Plan) (R.ReactMlT Identity) ()
window = do
    s <- ask
    lift $ R.lf (s ^. component . to JE.toJS)
        [ ("key",  s ^. key . to JE.toJS)
        , ("render", s ^. onRender . to JE.toJS)
        ]

-- | Internal rendering used by the React render callback
render :: G.WindowT (R.Scene Model Plan) (R.ReactMlT Identity) ()
render = do
    s <- ask
    lift $ R.lf (JE.strJS "input")
                    [ ("key", s ^. key . to JE.toJS)
                    , ("className", s ^. className . to JE.toJS)
                    , ("placeholder", s ^. placeholder . to JE.toJS)
                    , ("autoFocus", JE.toJS True)
                    , ("onKeyDown", s ^. onKeyDown . to JE.toJS)
                    ]

whenKeyDown :: J.JSVal -> MaybeT IO (Maybe J.JSString, J.JSVal)
whenKeyDown evt = do
        evt' <- MaybeT $ JE.fromJS evt
        evt'' <- MaybeT $ R.parseKeyboardEvent evt'
        evt''' <- lift $ R.parseEvent $ evt'
        -- target is the "input" DOM
        input <- lift $ pure . JE.toJS . R.target $ evt'''
        let k = R.keyCode evt''
        case k of
            -- FIXME: ESCAPE_KEY
            27 -> pure (Nothing, input)
            -- FIXME: ENTER_KEY
            13 -> do
                v <- MaybeT $ JE.getProperty "value" input >>= JE.fromJS
                pure (Just v, input)
            _ -> empty

onKeyDown' :: J.JSVal -> MaybeT IO [Action]
onKeyDown' = R.eventHandlerM whenKeyDown goLazy
  where
    goLazy :: (Maybe J.JSString, J.JSVal) -> MaybeT IO [Action]
    goLazy (ms, j) = pure $
        SendCommandsAction [SetPropertyCommand ("value", JE.toJS J.empty) j]
        : maybe [] (pure . SubmitAction) ms

-- | State update logic.
-- The best practice is to leave this in general Monad m (eg, not MonadIO).
-- This allows gadget to use STM as the base monad which allows for combining concurrently
-- with other stateful STM effects and still maintain a single source of truth.
gadget :: G.GadgetT Action (R.Gizmo Model Plan) Identity (D.DList Command)
gadget = do
    a <- ask
    case a of
        SendCommandsAction cmds -> pure $ D.fromList cmds

        -- parent widgets should detect this case to do something with submitted action
        SubmitAction _ -> pure empty

        InputRefAction v -> do
            inputRef .= v
            pure mempty
