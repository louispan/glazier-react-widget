{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Widget.Input (
    -- * Events
    OnBlur(..)
    , OnFocus(..)
    , OnEsc(..)
    , OnEnter(..)
    , OnToggle(..)
    -- * Text input
    , textInput
    -- ** Text input handlers
    , hdlInputFocusInput
    -- , hdlInputUpdateValue
    -- * Checkbox input
    , checkboxInput
    , IndeterminateCheckboxInput(..)
    , indeterminateCheckboxInput
    ) where

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

-- | This event is fired when the input loses focus (eg. from TAB or mouse click else where)
-- The model value is updated, and a stale will be called
-- immediately after his event, so the handler of this event
-- may update the model before that happens.
data OnBlur = OnBlur R.GadgetId JE.JSRep

-- | No data is changed, so stale is not called.
data OnFocus = OnFocus R.GadgetId JE.JSRep

-- | This event is fired when ESC is pressed.
-- Before this happens, js.blur() is called so InputDidBlur will be called.
-- This means the model value will have been set to the HTML value by the onBlur handler.
-- The handler of the event may wish to set the model value to some previously saved value.
data OnEsc = OnEsc R.GadgetId JE.JSRep

-- | This event is fired when enter is pressed.
-- Unlike InputDidEsc, blur is not called, but the model value is updated just
-- before this event is fired.
data OnEnter = OnEnter R.GadgetId JE.JSRep

-- | Called after checkbox is toggled.
data OnToggle = OnToggle R.GadgetId JE.JSRep

----------------------------------------

-- | Focus and start editing the input.
hdlInputFocusInput :: ( R.MonadReactor m
    , R.MonadJS m
    , R.MonadHTMLElement m)
    => R.GadgetId -> R.SceneHandler m v J.JSString () (Which '[])
hdlInputFocusInput i this@(R.Obj ref its) _ = R.terminate' . lift $ do
    -- Do a stale because the inputValue may have been modified by
    -- prior firing of this event, or other state changed that will affect the rendering
    -- of this input element.
    -- Only focus after rendering changed because we are using uncontrolled components
    -- with a new key. This will result in a different input element after each render
    R.addOnceOnUpdated this $ do
        obj <- R.doReadIORef ref
        void $ runMaybeT $ do
            j <- MaybeT . pure $ obj ^. its.R.plan.field @"refs".at i
            lift $ R.doSetProperty ("value", JE.toJSR J.empty) j
            lift $ R.focusRef i this
    R.stale this

-- hdlInputUpdateValue :: ( R.MonadReactor m
--     , R.MonadJS m)
--     => R.GadgetId -> R.SceneHandler m v J.JSString () (Which '[])
-- hdlInputUpdateValue i this@(R.Obj ref its) _ = R.terminate' . lift $ do
--     obj <- R.doReadIORef ref
--     void $ runMaybeT $ do
--         j <- MaybeT . pure $ obj ^. its.R.plan.field @"refs".at i
--         lift $ updateInputValue this (JE.toJSR j)

-- | Internal
updateInputValue :: (R.MonadReactor m, R.MonadJS m)
    => R.Scene m v J.JSString -> JE.JSRep -> m ()
updateInputValue this@(R.Obj ref its) j = do
    v <- JE.fromJSR @J.JSString <$> (R.doGetProperty "value" j)
    let v' = J.strip $ fromMaybe J.empty v
    R.doModifyIORef' ref (its.R.model .~ v')
    R.stale this

-- | Text inputs dosn't interact will as a controlled component,
-- so this prototype implements using the uncontrolled component.
textInput ::
    ( R.MonadReactor m
    , R.MonadJS m
    , R.MonadHTMLElement m
    )
    => R.GadgetId
    -> R.Prototype m v J.JSString (Which '[OnFocus, OnBlur, OnEnter, OnEsc])
textInput i = R.nulPrototype
    { R.display = \s -> R.lf' i s "input"
        -- For uncontrolled components, we need to generate a new key per render
        -- in to force react to use the new defaultValue
        [ ("key", JE.toJSR $ J.unwords
            [ R.runReactKey . R.reactKey $ s ^. R.plan
            , J.pack . show . R.currentFrameNum $ s ^. R.plan
            ])
        -- use the defaultValue to set the current html text
        -- "value" cannot be used as React will take over as a controlled component.
        , ("defaultValue", JE.toJSR $ s ^. R.model)
        ]
    , R.initializer = R.withRef i
        `R.andInitializer` onFocus
        `R.andInitializer` onBlur
        `R.andInitializer` onKeyDown
    }
  where
    onFocus :: ( R.MonadReactor m)
        => R.SceneInitializer m v s (Which '[OnFocus])
    onFocus = R.trigger i "onBlur" (pure) (pickOnly . OnFocus i)

    onBlur :: ( R.MonadReactor m, R.MonadJS m)
        => R.SceneInitializer m v J.JSString (Which '[OnBlur])
    onBlur = R.trigger' i "onBlur" (pure)
            `R.handledBy` hdlBlur

    hdlBlur :: (R.MonadReactor m, R.MonadJS m)
        => R.SceneHandler m v J.JSString JE.JSRep (Which '[OnBlur])
    hdlBlur this j = ContT $ \fire -> do
        updateInputValue this j
        -- fire so handler may change the model value if necessary
        fire . pickOnly $ OnBlur i j

    onKeyDown ::
        ( R.MonadReactor m
        , R.MonadHTMLElement m
        , R.MonadJS m
        ) => R.SceneInitializer m v J.JSString (Which '[OnEnter, OnEsc])
    onKeyDown = R.trigger' i "onKeyDown" (runMaybeT . R.fireKeyDownKey)
            `R.handledBy` R.maybeHandle hdlKeyDown

    hdlKeyDown ::
        ( R.MonadReactor m
        , R.MonadHTMLElement m
        , R.MonadJS m
        )
        => R.SceneHandler m v J.JSString R.KeyDownKey (Which '[OnEnter, OnEsc])
    hdlKeyDown this (R.KeyDownKey j key) = ContT $ \fire ->
        case key of
            "Enter" -> do
                updateInputValue this (JE.toJSR j)
                fire . pick $ OnEnter i (JE.toJSR j)
            "Escape" -> do
                R.blurRef i this -- The onBlur handler will also update the model
                fire . pick $ OnEsc i (JE.toJSR j)
            _ -> updateInputValue this (JE.toJSR j) -- copy the value but don't stale!

----------------------------------------

-- | This provide a prototype of a checkbox input but without a builder.
-- Instead a lens to the CheckboxInput is used, and the user of this widget
-- is responsible for making the entire model.
checkboxInput ::
    ( R.MonadReactor m
    , R.MonadHTMLElement m
    )
    => R.GadgetId
    -> R.Prototype m v Bool (Which '[OnFocus, OnBlur, OnEsc, OnToggle])
checkboxInput i = R.nulPrototype
    { R.display = \s -> R.lf' i s "input"
        [ ("key", JE.toJSR . R.reactKey $ s ^. R.plan)
        , ("type", "checkbox")
        , ("checked", JE.toJSR $ s ^. R.model)
        ]
    , R.initializer = R.withRef i
        `R.andInitializer` onFocus
        `R.andInitializer` onBlur
        `R.andInitializer` onKeyDown
        `R.andInitializer` onChange
    }

  where
    onChange ::
        ( R.MonadReactor m
        ) => R.SceneInitializer m v Bool (Which '[OnToggle])
    onChange = R.trigger' i "onChange" pure
            `R.handledBy` hdlChange

    hdlChange ::
        (R.MonadReactor m)
        => R.SceneHandler m v Bool JE.JSRep (Which '[OnToggle])
    hdlChange this@(R.Obj ref its) j = ContT $ \fire -> do
        R.doModifyIORef' ref (its.R.model %~ not)
        fire . pick $ OnToggle i (JE.toJSR j)
        R.stale this

    onFocus :: ( R.MonadReactor m)
        => R.SceneInitializer m v Bool (Which '[OnFocus])
    onFocus = R.trigger i "onFocus" pure (pickOnly . OnFocus i)

    onBlur :: ( R.MonadReactor m)
        => R.SceneInitializer m v Bool (Which '[OnBlur])
    onBlur = R.trigger i "onBlur" pure (pickOnly . OnBlur i)

    onKeyDown ::
        ( R.MonadReactor m
        , R.MonadHTMLElement m
        ) => R.SceneInitializer m v Bool (Which '[OnEsc, OnToggle])
    onKeyDown = R.trigger' i "onKeyDown" (runMaybeT . R.fireKeyDownKey)
            `R.handledBy` R.maybeHandle hdlKeyDown

    hdlKeyDown ::
        ( R.MonadReactor m
        , R.MonadHTMLElement m
        )
        => R.SceneHandler m v Bool R.KeyDownKey (Which '[OnEsc, OnToggle])
    hdlKeyDown this@(R.Obj ref its) (R.KeyDownKey j key) = ContT $ \fire ->
        case key of
            "Enter" -> do
                R.doModifyIORef' ref (its.R.model %~ not)
                fire . pick $ OnToggle i (JE.toJSR j)
                R.stale this
            "Escape" -> do
                R.blurRef i this
                fire . pick $ OnEsc i (JE.toJSR j)
                R.stale this
            "Space" -> do
                R.doModifyIORef' ref (its.R.model %~ not)
                fire . pick $ OnToggle i (JE.toJSR j)
                R.stale this
            _ -> pure () -- ^ NB. HTML input value has changed, do nothing extra

data IndeterminateCheckboxInput = IndeterminateCheckboxInput
    { checked :: Bool
    , indeterminate' :: Bool
    } deriving G.Generic

-- | This provide a prototype of a checkbox input but without a builder.
-- Instead a lens to the CheckboxInput is used, and the user of this widget
-- is responsible for making the entire model.
indeterminateCheckboxInput ::
    ( R.MonadReactor m
    , R.MonadJS m
    , R.MonadHTMLElement m
    )
    => R.GadgetId
    -> R.Prototype m v IndeterminateCheckboxInput (Which '[OnFocus, OnBlur, OnEsc, OnToggle])
indeterminateCheckboxInput i = R.magnifyPrototype (field @"checked") (checkboxInput i)
    & R.modifyInitializer fini
  where
    fini ini = ini `R.andInitializer` onActivated

    -- | Add setting the indeterminate' after every stale as this is the only
    -- way to change that setting.
    onActivated ::
        ( R.MonadReactor m
        , R.MonadJS m
        ) => R.SceneInitializer m v IndeterminateCheckboxInput (Which '[])
    onActivated this@(R.Obj ref its) = R.terminate' $ lift $ R.addEveryOnUpdated this go
      where
        go = do
            obj <- R.doReadIORef ref
            let j = obj ^. its.R.plan.field @"refs".at i
                f g = maybe (pure ()) g j
            f $ R.doSetProperty
                    ( "indeterminate'"
                    , JE.toJSR $ obj ^. its.R.model.field @"indeterminate'")
