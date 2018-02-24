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
import qualified Glazier.React as R
import qualified Glazier.React.Framework as R
import qualified JavaScript.Extras as JE

newtype TextInput = TextInput
    { value :: J.JSString
    } deriving G.Generic

-- | Fire this event to focus and start editing the input.
data FocusInput = FocusInput

hdlFocusInput ::
    ( R.MonadReactor m
    , R.MonadJS m
    , R.MonadHTMLElement m)
    => R.GadgetId -> R.SceneHandler m v TextInput (Which '[FocusInput]) (Which '[])
hdlFocusInput i this@(R.Obj ref its) _ = R.terminate' $ lift $ do
    -- Do a rerender because the inputValue may have been modified by
    -- prior firing of this event, or other state changed that will affect the rendering
    -- of this input element.
    -- Only focus after rendering changed because we are using uncontrolled components
    -- with a new key. This will result in a different input element after each render
    R.rerender this $ do
        obj <- R.doReadIORef ref
        void $ runMaybeT $ do
            j <- MaybeT . pure $ obj ^. its.R.plan.field @"refs".at i
            lift $ R.doSetProperty ("value", JE.toJSR J.empty) j
            lift $ R.focusRef i this

-- | Fire this event to update the model with the HTML input value.
data UpdateInput = UpdateInput

updateInputValue :: (R.MonadReactor m, R.MonadJS m)
    => R.Scene m v TextInput -> JE.JSRep -> m ()
updateInputValue (R.Obj ref its) j = do
    v <- JE.fromJSR @J.JSString <$> (R.doGetProperty "value" j)
    let v' = J.strip $ fromMaybe J.empty v
    R.doModifyIORef' ref (its.R.model.field @"value" .~ v')

hdlUpdateInput ::
    ( R.MonadReactor m
    , R.MonadJS m)
    => R.GadgetId -> R.SceneHandler m v TextInput (Which '[UpdateInput]) (Which '[])
hdlUpdateInput i this@(R.Obj ref its) _ = R.terminate' $ lift $ do
    obj <- R.doReadIORef ref
    void $ runMaybeT $ do
        j <- MaybeT . pure $ obj ^. its.R.plan.field @"refs".at i
        lift $ updateInputValue this (JE.toJSR j)
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
-- Unlike InputDidEsc, blur is not called
data InputDidEnter = InputDidEnter

-- | Called after checkbox is toggled
data CheckboxDidToggle = CheckboxDidToggle

-- | Text inputs dosn't interact will as a controlled component,
-- so this prototype implements using the uncontrolled component.
textInput ::
    ( R.MonadReactor m
    , R.MonadJS m
    , R.MonadHTMLElement m
    )
    => (i -> TextInput)
    -> Lens' s TextInput
    -> R.GadgetId
    -> R.Prototype m v i s
        (Many '[TextInput])
        (Many '[TextInput])
        (Which '[InputDidBlur, InputDidEnter, InputDidEsc])
        (Which '[FocusInput, UpdateInput])
        (Which '[])
textInput fi fs i =
    let p = R.nulPrototype
            { R.display = \s -> R.lf' i s "input"
                -- For uncontrolled components, we need to generate a new key per render
                -- in to force react to use the new defaultValue
                [ ("key", JE.toJSR $ J.unwords
                    [ R.runReactKey . R.reactKey $ s ^. R.plan
                    , J.pack . show . R.frameNum $ s ^. R.plan
                    ])
                -- use the defaultValue to set the current html text
                -- "value" cannot be used as React will take over as a controlled component.
                , ("defaultValue", JE.toJSR $ s ^. R.model.field @"value")
                ]
            , R.builder = R.build @TextInput
            , R.activator = R.withRef i `R.andActivator` onBlur `R.andActivator` onKeyDown
            , R.handler = hdlFocusInput i `R.orHandler` hdlUpdateInput i
            }
    in R.toItemPrototype fi fs p
  where
    onBlur :: ( R.MonadReactor m, R.MonadJS m)
        => R.SceneActivator m v TextInput (Which '[InputDidBlur])
    onBlur = R.trigger' i "onBlur" (pure)
            `R.activates` hdlBlur

    hdlBlur :: (R.MonadReactor m, R.MonadJS m)
        => R.SceneHandler m v TextInput JE.JSRep (Which '[InputDidBlur])
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
                fire . pick $ InputDidEnter
                R.rerender' this
            "Escape" -> do
                R.blurRef i this -- The onBlur handler will also update the model
                fire . pick $ InputDidEsc
                R.rerender' this
            _ -> pure () -- ^ NB. HTML input value has changed, do nothing extra


data CheckboxInput = CheckboxInput
    { checked :: Bool
    , indeterminate :: Bool
    } deriving G.Generic

-- | This provide a prototype of a checkbox input but without a builder.
-- Instead a lens to the CheckboxInput is used, and the user of this widget
-- is responsible for making the entire model.
checkboxInput ::
    ( R.MonadReactor m
    , R.MonadJS m
    , R.MonadHTMLElement m
    )
    => R.GadgetId
    -> R.Prototype m v i CheckboxInput
        (Many '[])
        (Many '[])
        (Which '[InputDidBlur, InputDidEnter, InputDidEsc, CheckboxDidToggle])
        (Which '[])
        (Which '[])
checkboxInput i =
    let p = R.nulPrototype
            { R.display = \s -> R.lf' i s "input"
                [ ("key", JE.toJSR . R.reactKey $ s ^. R.plan)
                , ("type", "checkbox")
                , ("checked", JE.toJSR $ s ^. R.model.field @"checked")
                ]
            , R.activator = R.withRef i
                `R.andActivator` onActivated
                `R.andActivator` onBlur
                `R.andActivator` onKeyDown
            }
    in p
  where
    -- | Add setting the indeterminate after every rerender as this is the only
    -- way to change that setting.
    onActivated ::
        ( R.MonadReactor m
        , R.MonadJS m
        ) => R.SceneActivator m v CheckboxInput (Which '[])
    onActivated (R.Obj ref its) = R.terminate' $ lift $ R.doModifyIORef' ref $ its.R.plan.field @"everyOnUpdated" %~ (*> go)
      where
        go = do
            obj <- R.doReadIORef ref
            let j = obj ^. its.R.plan.field @"refs".at i
                f g = maybe (pure ()) g j
            f $ R.doSetProperty
                    ( "indeterminate"
                    , JE.toJSR $ obj ^. its.R.model.field @"indeterminate")

    onBlur :: ( R.MonadReactor m)
        => R.SceneActivator m v CheckboxInput (Which '[InputDidBlur])
    onBlur = R.trigger' i "onBlur" (const $ pure ())
            `R.activates` hdlBlur

    hdlBlur :: (R.MonadReactor m)
        => R.SceneHandler m v CheckboxInput () (Which '[InputDidBlur])
    hdlBlur this _ = ContT $ \fire -> do
        fire . pickOnly $ InputDidBlur
        R.rerender' this

    onKeyDown ::
        ( R.MonadReactor m
        , R.MonadHTMLElement m
        ) => R.SceneActivator m v CheckboxInput (Which '[InputDidEnter, InputDidEsc, CheckboxDidToggle])
    onKeyDown = R.trigger' i "onKeyDown" (runMaybeT . R.fireKeyDownKey)
            `R.activates` R.maybeHandle hdlKeyDown

    hdlKeyDown ::
        ( R.MonadReactor m
        , R.MonadHTMLElement m
        )
        => R.SceneHandler m v CheckboxInput R.KeyDownKey (Which '[InputDidEnter, InputDidEsc, CheckboxDidToggle])
    hdlKeyDown this@(R.Obj ref its) (R.KeyDownKey _ key) = ContT $ \fire ->
        case key of
            "Enter" -> do
                fire . pick $ InputDidEnter
                R.rerender' this
            "Escape" -> do
                R.blurRef i this
                fire . pick $ InputDidEsc
                R.rerender' this
            "Space" -> do
                R.blurRef i this
                R.doModifyIORef' ref (its.R.model.field @"checked" %~ not)
                fire . pick $ CheckboxDidToggle
                R.rerender' this
            _ -> pure () -- ^ NB. HTML input value has changed, do nothing extra
