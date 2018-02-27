{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Widget.Input (
    -- * Events
    OnChange(..)
    -- * Text input
    , textInput
    -- ** Text input handlers
    , focusInput
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

data OnChange = OnChange R.GadgetId JE.JSRep

----------------------------------------

-- | Focus and start editing the input.
focusInput :: ( R.MonadReactor m
    , R.MonadJS m
    , R.MonadHTMLElement m)
    => R.GadgetId -> R.Scene m v J.JSString -> ContT () m (Which '[])
focusInput i this@(R.Obj ref its) = R.terminate' . lift $ do
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

-- -- | Internal
-- updateInputValue :: (R.MonadReactor m, R.MonadJS m)
--     => R.Scene m v J.JSString -> JE.JSRep -> m ()
-- updateInputValue this@(R.Obj ref its) j = do
--     v <- JE.fromJSR @J.JSString <$> (R.doGetProperty "value" j)
--     let v' = J.strip $ fromMaybe J.empty v
--     R.doModifyIORef' ref (its.R.model .~ v')
--     R.stale this

-- | Text inputs dosn't interact well as a controlled component.
-- Eg. cursor jumps if user types quickly.
-- I think there a timing issue with lazy event handling setting the value,
-- So this prototype implements using the uncontrolled component
-- (using defaultValue instead of value).
-- For input, React uses controlled input if input.value is not null
-- For checkboxes,  React uses controlled checkbox if input.checked is not null
-- https://stackoverflow.com/questions/37427508/react-changing-an-uncontrolled-input
textInput ::
    ( R.MonadReactor m
    , R.MonadJS m
    )
    => R.GadgetId
    -> R.Prototype m v J.JSString (Which '[OnChange])
textInput i = R.nulPrototype
    { R.display = \s -> R.lf' i s "input"
        [ ("key", JE.toJSR . R.reactKey $ s ^. R.plan)
        -- "value" cannot be used as React will take over as a controlled component.
        -- use the defaultValue to set the *initial* html text
        , ("defaultValue", JE.toJSR $ s ^. R.model)
        ]
    , R.initializer = R.withRef i
        `R.andInitializer` onInitialized
        `R.andInitializer` onChange
    }
  where
    -- | set the DOM input value manually after every rerender.
    onInitialized ::
        ( R.MonadReactor m
        , R.MonadJS m
        ) => R.SceneInitializer m v J.JSString (Which '[])
    onInitialized this@(R.Obj ref its) = R.terminate' $ lift $ R.addEveryOnUpdated this go
      where
        go = do
            obj <- R.doReadIORef ref
            let j = obj ^. its.R.plan.field @"refs".at i
                f g = maybe (pure ()) g j
            f $ R.doSetProperty
                    ( "value'"
                    , JE.toJSR $ obj ^. its.R.model)

    onChange ::
        ( R.MonadReactor m
        , R.MonadJS m
        ) => R.SceneInitializer m v J.JSString (Which '[OnChange])
    onChange = R.trigger' i "onChange" pure
            `R.handledBy` hdlChange

    hdlChange ::
        ( R.MonadReactor m, R.MonadJS m
        ) => R.SceneHandler m v J.JSString JE.JSRep (Which '[OnChange])
    hdlChange (R.Obj ref its) j = ContT $ \fire -> do
        v <- JE.fromJSR @J.JSString <$> (R.doGetProperty "value" j)
        let v' = fromMaybe J.empty v
        R.doModifyIORef' ref (its.R.model .~ v')
        fire . pick $ OnChange i (JE.toJSR j)
        -- Don't mark input as stale

----------------------------------------

-- | This provide a prototype of a checkbox input but without a builder.
-- Instead a lens to the CheckboxInput is used, and the user of this widget
-- is responsible for making the entire model.
checkboxInput ::
    ( R.MonadReactor m
    )
    => R.GadgetId
    -> R.Prototype m v Bool (Which '[OnChange])
checkboxInput i = R.nulPrototype
    { R.display = \s -> R.lf' i s "input"
        [ ("key", JE.toJSR . R.reactKey $ s ^. R.plan)
        , ("type", "checkbox")
        , ("checked", JE.toJSR $ s ^. R.model)
        ]
    , R.initializer = R.withRef i
        `R.andInitializer` onChange
    }

  where
    onChange ::
        ( R.MonadReactor m
        ) => R.SceneInitializer m v Bool (Which '[OnChange])
    onChange = R.trigger' i "onChange" pure
            `R.handledBy` hdlChange

    hdlChange ::
        (R.MonadReactor m)
        => R.SceneHandler m v Bool JE.JSRep (Which '[OnChange])
    hdlChange this@(R.Obj ref its) j = ContT $ \fire -> do
        R.doModifyIORef' ref (its.R.model %~ not)
        fire . pick $ OnChange i (JE.toJSR j)
        R.stale this

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
    )
    => R.GadgetId
    -> R.Prototype m v IndeterminateCheckboxInput (Which '[OnChange])
indeterminateCheckboxInput i = R.magnifyPrototype (field @"checked") (checkboxInput i)
    & R.modifyInitializer fini
  where
    fini ini = ini `R.andInitializer` onInitialized

    -- | Add setting the indeterminate' after every stale as this is the only
    -- way to change that setting.
    onInitialized ::
        ( R.MonadReactor m
        , R.MonadJS m
        ) => R.SceneInitializer m v IndeterminateCheckboxInput (Which '[])
    onInitialized this@(R.Obj ref its) = R.terminate' $ lift $ R.addEveryOnUpdated this go
      where
        go = do
            obj <- R.doReadIORef ref
            let j = obj ^. its.R.plan.field @"refs".at i
                f g = maybe (pure ()) g j
            f $ R.doSetProperty
                    ( "indeterminate'"
                    , JE.toJSR $ obj ^. its.R.model.field @"indeterminate'")
