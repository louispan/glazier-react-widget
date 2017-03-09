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
    , Gasket(..)
    , HasGasket(..)
    , mkGasket
    , Model(..)
    , HasModel(..)
    , mkSuperModel
    , Widget
    , GModel
    , MModel
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
import qualified GHCJS.Marshal as J
import qualified GHCJS.Marshal.Pure as J
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
    | SubmitCommand J.JSString
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

data Gasket = Gasket
    { _component :: R.ReactComponent
    , _onRender :: J.Callback (J.JSVal -> IO J.JSVal)
    , _onInputRef :: J.Callback (J.JSVal -> IO ())
    , _onKeyDown :: J.Callback (J.JSVal -> IO ())
    } deriving (G.Generic)

makeClassyPrisms ''Action
makeClassy ''Gasket
makeClassy ''Model

mkGasket :: R.MModel Gasket Model -> F (R.Maker (Action act)) Gasket
mkGasket mm = Gasket
    <$> R.getComponent
    <*> (R.mkRenderer mm $ const render)
    <*> (R.mkHandler $ pure . pure . InputRefAction)
    <*> (R.mkHandler onKeyDown')

instance CD.Disposing Model where
    disposing _ = CD.DisposeNone

mkSuperModel :: Model -> F (R.Maker (Action act)) (SuperModel act)
mkSuperModel s = R.mkSuperModel mkGasket $ \gkt -> R.GModel gkt s

data Widget act
instance R.IsWidget (Widget act) where
    type WidgetAction (Widget act) = Action act
    type WidgetCommand (Widget act) = Command act
    type WidgetModel (Widget act) = Model
    type WidgetGasket (Widget act) = Gasket
type GModel act = R.WidgetGModel (Widget act)
type MModel act = R.WidgetMModel (Widget act)
type SuperModel act = R.WidgetSuperModel (Widget act)
instance CD.Disposing Gasket
instance HasGasket (R.GModel Gasket Model) where
    gasket = R.widgetGasket
instance HasModel (R.GModel Gasket Model) where
    model = R.widgetModel
instance HasGasket (R.SuperModel Gasket Model) where
    gasket = R.gModel . gasket
instance HasModel (R.SuperModel Gasket Model) where
    model = R.gModel . model

-- | This is used by parent components to render this component
window :: Monad m => G.WindowT (GModel act) (R.ReactMlT m) ()
window = do
    s <- ask
    lift $ R.lf (s ^. component . to J.jsval)
        [ ("key",  s ^. uid . to J.jsval)
        , ("render", s ^. onRender . to J.jsval)
        ]

-- | This is used by the React render callback
render :: Monad m => G.WindowT (GModel act) (R.ReactMlT m) ()
render = do
    s <- ask
    lift $ R.lf (JE.strval "input")
                    [ ("key", s ^. uid . to J.jsval)
                    , ("className", s ^. className . to J.jsval)
                    , ("placeholder", s ^. placeholder . to J.jsval)
                    , ("autoFocus", J.pToJSVal True)
                    , ("onKeyDown", s ^. onKeyDown . to J.jsval)
                    ]

whenKeyDown :: J.JSVal -> MaybeT IO (Maybe J.JSString, J.JSVal)
whenKeyDown evt = do
        evt' <- MaybeT $ pure $ R.castSyntheticEvent evt
        evt'' <- MaybeT $ pure $ R.parseKeyboardEvent evt'
        -- target is the "input" DOM
        input <- lift $ pure . J.jsval . R.target . R.parseEvent $ evt'
        let k = R.keyCode evt''
        case k of
            -- FIXME: ESCAPE_KEY
            27 -> pure $ (Nothing, input)
            -- FIXME: ENTER_KEY
            13 -> do
                v <- MaybeT $ JE.getProperty "value" input >>= J.fromJSVal
                pure $ (Just v, input)
            _ -> empty

onKeyDown' :: J.JSVal -> MaybeT IO [Action act]
onKeyDown' = R.eventHandlerM whenKeyDown goLazy
  where
    goLazy :: (Maybe J.JSString, J.JSVal) -> MaybeT IO [Action act]
    goLazy (ms, j) = pure $
        SendCommandsAction [SetPropertyCommand ("value", J.jsval J.empty) j]
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

        SubmitAction v -> do
            let v' = J.strip v
            if J.null v'
                then pure mempty
                else pure $ D.singleton $ SubmitCommand v'

        InputRefAction v -> do
            inputRef .= v
            pure mempty

        GetPropertyAction prop f -> do
            j <- use inputRef
            pure $ D.singleton $ GetPropertyCommand prop j f
