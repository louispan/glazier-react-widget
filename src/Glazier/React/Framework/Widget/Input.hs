{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Framework.Widget.Input where

import Control.Lens
import Control.Monad.Cont
import Control.Monad.Trans.Maybe
import Data.Diverse.Lens
import Data.Generics.Product
import qualified Data.JSString as J
import Data.Maybe
import Data.Semigroup
import qualified GHCJS.Types as J
import qualified Glazier.React as R
import qualified Glazier.React.Framework as R
import qualified JavaScript.Extras as JE

-- Fire this event to focus and start editing the input.
data FocusInput = FocusInput

-- -- Fire this event to update the value in the text input
-- data UpdateInput = UpdateInput J.JSString

-- | This event is fired when the input loses focus (eg. from TAB or mouse click else where)
-- The model input value is updated, and a rerender will be called
-- immediately after his event, so the handler of this event
-- may update the model before that happens.
data InputDidBlur = InputDidBlur

-- | This event is fired when ESC is pressed.
-- Before this happens, js.blur() is called so InputDidBlur will be called.
-- This means the input model will have been set to the HTML value by the onBlur handleR.
-- It is up to the handler of this event to "restore" the input value to a previous value
-- if required.
data InputDidEsc = InputDidEsc

-- | This event is fired when enter is pressed.
-- Before this happens, js.blur() is called so InputDidBlur will be called.
-- This means the input model will have been set to the HTML value by the onBlur handleR.
data InputDidEnter = InputDidEnter

textInput ::
    ( R.MonadReactor m
    , R.MonadJS m
    , R.MonadHTMLElement m
    , HasItem' J.JSString s
    , HasItem' J.JSString i
    )
    => R.GadgetId
    -> (R.Frame m s -> [JE.Property])
    -> R.Prototype m v i s
        (Many '[J.JSString])
        (Many '[J.JSString])
        (Which '[InputDidBlur, InputDidEnter, InputDidEsc])
        (Which '[FocusInput])
        (Which '[])
textInput i props =
    let p = R.nulPrototype
            { R.builder = R.build @J.JSString
            , R.activator = R.withRef i `R.andActivator` onBlur `R.andActivator` onKeyDown
            , R.handler = hdlFocusInput }
    in (R.toItemPrototype p) {
        R.display = \s -> R.leaf "input" (R.getListeners i s)
            -- For uncontrolled components, we need to generate a new key per render
            -- in to force react to use the new defaultValue
            (props s <> [ ("key", JE.toJS' $ J.unwords
                [ R.runReactKey . R.reactKey $ s ^. R.plan
                , J.pack . show . R.frameNum $ s ^. R.plan
                ])
            -- use the defaultValue to set the current html text
            -- "value" cannot be used as React will take over as a controlled component.
            , ("defaultValue", JE.toJS' $ s ^. R.model.item' @J.JSString)])
    }
  where
    onBlur :: ( R.MonadReactor m, R.MonadJS m)
        => R.SceneActivator m v J.JSString (Which '[InputDidBlur])
    onBlur = R.trigger' i "onBlur" (pure)
            `R.activates` hdlBlur

    updateInputValue :: (R.MonadReactor m, R.MonadJS m)
        => R.Scene m v J.JSString -> J.JSVal -> m ()
    updateInputValue (R.Obj ref its) j = do
        v <- JE.fromJS' @J.JSString <$> (R.doGetProperty "value" (JE.toJS j))
        let v' = J.strip $ fromMaybe J.empty v
        R.doModifyIORef' ref (its.R.model .~ v')

    hdlBlur :: (R.MonadReactor m, R.MonadJS m)
        => R.SceneHandler m v J.JSString J.JSVal (Which '[InputDidBlur])
    hdlBlur this j = ContT $ \fire -> do
        updateInputValue this j
        -- fire so handler may change the model value if necessary
        fire . pickOnly $ InputDidBlur
        -- re-render using updated model
        R.rerender' this

    onKeyDown ::
        ( R.MonadReactor m
        , R.MonadHTMLElement m
        ) => R.SceneActivator m v J.JSString (Which '[InputDidEnter, InputDidEsc])
    onKeyDown = R.trigger' i "onKeyDown" (runMaybeT . R.fireKeyDownKey)
            `R.activates` R.maybeHandle hdlKeyDown

    hdlKeyDown ::
        ( R.MonadReactor m
        , R.MonadHTMLElement m
        )
        => R.SceneHandler m v J.JSString R.KeyDownKey (Which '[InputDidEnter, InputDidEsc])
    hdlKeyDown this (R.KeyDownKey _ key) = ContT $ \fire ->
        case key of
            "Enter" -> do
                R.blurRef i this -- The onBlur handler will also update the model
                fire . pick $ InputDidEnter
                -- re-render using updated inputValue
                R.rerender' this
            "Escape" -> do
                R.blurRef i this -- The onBlur handler will also update the model
                fire . pick $ InputDidEsc
                -- re-render using updated inputValue
                R.rerender' this
            _ -> pure () -- ^ NB. HTML input value has changed, do nothing extra

    hdlFocusInput ::
        ( R.MonadReactor m
        , R.MonadJS m
        , R.MonadHTMLElement m)
        => R.SceneHandler m v J.JSString (Which '[FocusInput]) (Which '[])
    hdlFocusInput this@(R.Obj ref its) _ = R.terminate' $ lift $ do
        -- Do a rerender because the inputValue may have been modified by
        -- prior firing of this event, or other state changed that will affect the rendering
        -- of this input element.
        -- Only focus after rendering changed because we are using uncontrolled components
        -- with a new key. This will result in a different input element after each render
        R.rerender this $ do
            obj <- R.doReadIORef ref
            void $ runMaybeT $ do
                j <- MaybeT . pure $ obj ^. its.R.plan.field @"refs".at i
                lift $ R.doSetProperty ("value", JE.toJS' J.empty) (JE.toJS j)
                lift $ R.focusRef i this
