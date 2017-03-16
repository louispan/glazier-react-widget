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
import qualified Glazier.React.Widget as R
import qualified JavaScript.Extras as JE

data Command act
    = SetPropertyCommand JE.Property J.JSVal
    | GetPropertyCommand J.JSString J.JSVal (J.JSVal -> act)
    deriving Functor

data Action act
    = SendCommandsAction [Command act]
    | SubmitAction J.JSString
    | InputRefAction J.JSVal
    | GetPropertyAction J.JSString (J.JSVal -> act)
    deriving Functor

data Model = Model
    { _uid :: J.JSString
    , _inputRef :: J.JSVal
    , _placeholder :: J.JSString
    , _className :: J.JSString
    }

data Plan = Plan
    { _component :: R.ReactComponent
    , _onRender :: J.Callback (J.JSVal -> IO J.JSVal)
    , _onInputRef :: J.Callback (J.JSVal -> IO ())
    , _onKeyDown :: J.Callback (J.JSVal -> IO ())
    } deriving (G.Generic)

makeClassyPrisms ''Action
makeClassy ''Plan
makeClassy ''Model

mkPlan :: R.Replica Model Plan -> F (R.Maker (Action act)) Plan
mkPlan mm = Plan
    <$> R.getComponent
    <*> (R.mkRenderer mm $ const render)
    <*> (R.mkHandler $ pure . pure . InputRefAction)
    <*> (R.mkHandler onKeyDown')

instance CD.Disposing Model where
    disposing _ = CD.DisposeNone

mkSuperModel :: Model -> F (R.Maker (Action act)) (SuperModel act)
mkSuperModel mdl = R.mkSuperModel mkPlan (R.Design mdl)

data Widget act
instance R.IsWidget (Widget act) where
    type WidgetAction (Widget act) = Action act
    type WidgetCommand (Widget act) = Command act
    type WidgetModel (Widget act) = Model
    type WidgetPlan (Widget act) = Plan
type Design act = R.WidgetDesign (Widget act)
type Replica act = R.WidgetReplica (Widget act)
type SuperModel act = R.WidgetSuperModel (Widget act)
instance CD.Disposing Plan
instance HasPlan (R.Design Model Plan) where
    plan = R.widgetPlan
instance HasModel (R.Design Model Plan) where
    model = R.widgetModel
instance HasPlan (R.SuperModel Model Plan) where
    plan = R.design . plan
instance HasModel (R.SuperModel Model Plan) where
    model = R.design . model

-- | This is used by parent components to render this component
window :: Monad m => G.WindowT (Design act) (R.ReactMlT m) ()
window = do
    s <- ask
    lift $ R.lf (s ^. component . to JE.toJS)
        [ ("key",  s ^. uid . to JE.toJS)
        , ("render", s ^. onRender . to JE.toJS)
        ]

-- | This is used by the React render callback
render :: Monad m => G.WindowT (Design act) (R.ReactMlT m) ()
render = do
    s <- ask
    lift $ R.lf (JE.strJS "input")
                    [ ("key", s ^. uid . to JE.toJS)
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
            27 -> pure $ (Nothing, input)
            -- FIXME: ENTER_KEY
            13 -> do
                v <- MaybeT $ JE.getProperty "value" input >>= JE.fromJS
                pure $ (Just v, input)
            _ -> empty

onKeyDown' :: J.JSVal -> MaybeT IO [Action act]
onKeyDown' = R.eventHandlerM whenKeyDown goLazy
  where
    goLazy :: (Maybe J.JSString, J.JSVal) -> MaybeT IO [Action act]
    goLazy (ms, j) = pure $
        SendCommandsAction [SetPropertyCommand ("value", JE.toJS J.empty) j]
        : maybe [] (pure . SubmitAction) ms

-- | State update logic.
-- The best practice is to leave this in general Monad m (eg, not MonadIO).
-- This allows gadget to use STM as the base monad which allows for combining concurrently
-- with other stateful STM effects and still maintain a single source of truth.
gadget :: Monad m => G.GadgetT (Action act) (SuperModel act) m (D.DList (Command act))
gadget = do
    a <- ask
    case a of
        SendCommandsAction cmds -> pure $ D.fromList cmds

        -- parent widgets should detect this case to do something with submitted action
        SubmitAction _ -> pure empty

        InputRefAction v -> do
            inputRef .= v
            pure mempty

        GetPropertyAction prop f -> do
            j <- use inputRef
            pure $ D.singleton $ GetPropertyCommand prop j f
