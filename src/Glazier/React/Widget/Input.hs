{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Widget.Input where

import Control.Lens
import Control.Monad.Cont
import Control.Monad.Trans.Maybe
import Data.Diverse.Lens
import Data.Generics.Product
import qualified Data.JSString as J
import Data.Maybe
import qualified GHC.Generics as G
import qualified GHCJS.Types as J
import qualified Glazier.React as R
import qualified Glazier.React.Framework as R
import qualified JavaScript.Extras as JE

newtype TextInput = TextInput
    { value :: J.JSString
    } deriving G.Generic

-- | Fire this event to focus and start editing the input.
data FocusInput = FocusInput

-- | This event is fired when the input loses focus (eg. from TAB or mouse click else where)
-- The model value is updated, and a rerender will be called
-- immediately after his event, so the handler of this event
-- may update the model before that happens.
data InputDidBlur = InputDidBlur

-- | This event is fired when ESC is pressed.
-- Before this happens, js.blur() is called so InputDidBlur will be called.
-- This means the model value will have been set to the HTML value by the onBlur handler.
-- The handler of the event may wish to set the model value to some previously saved value.
data InputDidEsc = InputDidEsc

-- | This event is fired when enter is pressed.
-- Before this happens, js.blur() is called so InputDidBlur will be called.
-- This means the input model will have been set to the HTML value by the onBlur handler.
data InputDidEnter = InputDidEnter

-- | This provide a prototype of a text input but without a builder.
-- Instead a lens to the input J.JSString is used, and the user of this widget
-- is responsible for making the entire model.
textInput ::
    ( R.MonadReactor m
    , R.MonadJS m
    , R.MonadHTMLElement m
    )
    => R.GadgetId
    -> (i -> TextInput)
    -> Lens' s TextInput
    -> R.Prototype m v i s
        (Many '[TextInput])
        (Many '[TextInput])
        (Which '[InputDidBlur, InputDidEnter, InputDidEsc])
        (Which '[FocusInput])
        (Which '[])
textInput i fi fs =
    let p = R.nulPrototype
            { R.display = \s -> R.lf' i s "input"
                -- For uncontrolled components, we need to generate a new key per render
                -- in to force react to use the new defaultValue
                [ ("key", JE.toJS' $ J.unwords
                    [ R.runReactKey . R.reactKey $ s ^. R.plan
                    , J.pack . show . R.frameNum $ s ^. R.plan
                    ])
                -- use the defaultValue to set the current html text
                -- "value" cannot be used as React will take over as a controlled component.
                , ("defaultValue", JE.toJS' $ s ^. R.model.field @"value")
                ]
            , R.builder = R.build @TextInput
            , R.activator = R.withRef i `R.andActivator` onBlur `R.andActivator` onKeyDown
            , R.handler = hdlFocusInput
            }
    in R.toItemPrototype fi fs p
  where
    onBlur :: ( R.MonadReactor m, R.MonadJS m)
        => R.SceneActivator m v TextInput (Which '[InputDidBlur])
    onBlur = R.trigger' i "onBlur" (pure)
            `R.activates` hdlBlur

    updateInputValue :: (R.MonadReactor m, R.MonadJS m)
        => R.Scene m v TextInput -> J.JSVal -> m ()
    updateInputValue (R.Obj ref its) j = do
        v <- JE.fromJS' @J.JSString <$> (R.doGetProperty "value" (JE.toJS j))
        let v' = J.strip $ fromMaybe J.empty v
        R.doModifyIORef' ref (its.R.model.field @"value" .~ v')

    hdlBlur :: (R.MonadReactor m, R.MonadJS m)
        => R.SceneHandler m v TextInput J.JSVal (Which '[InputDidBlur])
    hdlBlur this j = ContT $ \fire -> do
        updateInputValue this j
        -- fire so handler may change the model value if necessary
        fire . pickOnly $ InputDidBlur
        -- re-render using updated model
        R.rerender' this

    onKeyDown ::
        ( R.MonadReactor m
        , R.MonadHTMLElement m
        ) => R.SceneActivator m v TextInput (Which '[InputDidEnter, InputDidEsc])
    onKeyDown = R.trigger' i "onKeyDown" (runMaybeT . R.fireKeyDownKey)
            `R.activates` R.maybeHandle hdlKeyDown

    hdlKeyDown ::
        ( R.MonadReactor m
        , R.MonadHTMLElement m
        )
        => R.SceneHandler m v TextInput R.KeyDownKey (Which '[InputDidEnter, InputDidEsc])
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
        => R.SceneHandler m v TextInput (Which '[FocusInput]) (Which '[])
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

data CheckboxInput = CheckboxInput
    { checked :: Bool
    , indeterminate :: Bool
    } deriving G.Generic

-- -- | This provide a prototype of a checkbox input but without a builder.
-- -- Instead a lens to the CheckboxInput is used, and the user of this widget
-- -- is responsible for making the entire model.
-- checkboxInput ::
--     ( R.MonadReactor m
--     , R.MonadJS m
--     , R.MonadHTMLElement m
--     )
--     => R.GadgetId
--     -> Lens' s CheckboxInput
--     -> (R.Frame m s -> [JE.Property])
--     -> R.Prototype m v i s
--         (Many '[])
--         (Many '[])
--         (Which '[])
--         (Which '[])
--         (Which '[])
-- checkboxInput i checkbox props =
--     let p = R.nulPrototype
--             -- { R.activator = R.withRef i `R.andActivator` onBlur `R.andActivator` onKeyDown
--             -- , R.handler = hdlFocusInput }
--     in (R.viaSpec checkbox p) {
--         R.display = \s -> R.lf' i s "input"
--             (props s <>
--                 [ ("key", R.runReactKey . R.reactKey $ s ^. R.plan)
--                 , ("type", "checkbox")
--                 , ("checked", JE.toJS' $ s ^. R.model.checkbox)
--                 ]
--             )
--     }

--   where
--     onBlur :: ( R.MonadReactor m, R.MonadJS m)
--         => R.SceneActivator m v J.JSString (Which '[InputDidBlur])
--     onBlur = R.trigger' i "onBlur" (pure)
--             `R.activates` hdlBlur
