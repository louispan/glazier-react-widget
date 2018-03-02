{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Widget.Input (
    -- * Text input
    textInput
    -- * Checkbox input
    , checkboxInput
    , IndeterminateCheckboxInput(..)
    , indeterminateCheckboxInput
    ) where

import Control.Lens
import Control.Monad.Cont
import Control.Monad.Trans.Maybe
import qualified Data.Algorithm.Diff as D
import Data.Diverse.Lens
import Data.Generics.Product
import qualified Data.JSString as J
import Data.Maybe
import qualified GHC.Generics as G
import qualified Glazier.React as Z
import qualified Glazier.React.Framework as Z
import qualified JavaScript.Extras as JE

----------------------------------------

-- | Text inputs dosn't interact well as a React controlled component.
-- Eg. cursor jumps if user types quickly.
-- I think there a timing issue with lazy event handlers setting the value,
-- So this prototype uses the React uncontrolled component
-- (using defaultValue instead of value).
--
-- For input, React uses controlled input if input.value is not null
-- For checkboxes,  React uses controlled checkbox if input.checked is not null
-- https://stackoverflow.com/questions/37427508/react-changing-an-uncontrolled-input
--
-- This widget attempts to set the cursor position at the correct place
-- by using a diffing algorithm on the old and new value.
--
-- Warning: This widget listens to onChange and will update the model value with the DOM input value.
-- potentially overridding any user changes.
-- So when changing the model value, be sure that the onChange handler will not be called.
textInput ::
    ( Z.MonadReactor m
    , Z.MonadJS m
    )
    => Z.GadgetId
    -> Z.Prototype m v J.JSString (Which '[])
textInput gid = Z.nulPrototype
    { Z.display = \s -> Z.lf' gid s "input"
        [ ("key", JE.toJSR . Z.reactKey $ s ^. Z.plan)
        -- "value" cannot be used as React will take over as a controlled component.
        -- The defaultValue only sets the *initial* DOM value
        -- The user will need to modify reactKey if they want
        -- react to actually rerender, since React will not do anything
        -- even if defaultValue changes.
        -- But hopefully this is not necessary as the DOM inpt value
        -- is updated under the hood in onInitialized
        , ("defaultValue", JE.toJSR $ s ^. Z.model)
        ]
    , Z.initializer = Z.withRef gid
        `Z.alsoInitializer` onInitialized
        `Z.alsoInitializer` (Z.trigger' gid "onChange" () `Z.handledBy` hdlChange)
    }
  where

    -- | Add setting the DOM input value after every render as this is the only
    -- way to change that setting.
    onInitialized ::
        ( Z.MonadReactor m
        , Z.MonadJS m
        ) => Z.SceneInitializer m v J.JSString (Which '[])
    onInitialized this@(Z.Obj ref its) = Z.terminate' $ lift $ Z.addEveryOnUpdated this go
      where
        go = do
            obj <- Z.doReadIORef ref
            let s = obj ^. its.Z.model
            void $ runMaybeT $ do
                j <- MaybeT $ pure $ obj ^. its.Z.plan.field @"refs".at gid
                start <- MaybeT $ JE.fromJSR <$> (Z.doGetProperty "selectionStart" j)
                end <- MaybeT $ JE.fromJSR <$> (Z.doGetProperty "selectionEnd" j)
                v <- MaybeT $ JE.fromJSR <$> (Z.doGetProperty "value" j)
                let (a, b) = estimateSelectionRange (J.unpack v) (J.unpack s) start end
                lift $ j & Z.doSetProperty ("value", JE.toJSR s)
                lift $ j & Z.doSetProperty ("selectionStart", JE.toJSR a)
                lift $ j & Z.doSetProperty ("selectionEnd", JE.toJSR b)

    hdlChange ::
        ( Z.MonadReactor m, Z.MonadJS m
        ) => Z.SceneHandler m v J.JSString a (Which '[])
    hdlChange (Z.Obj ref its) _ = Z.terminate' $ lift $ void $ runMaybeT $ do
        obj <- lift $ Z.doReadIORef ref
        j <- MaybeT $ pure $ obj ^. its.Z.plan.field @"refs".at gid
        v <- lift $ (fromMaybe J.empty . JE.fromJSR) <$> (Z.doGetProperty "value" j)
        lift $ Z.doModifyIORef' ref (its.Z.model .~ v)
        -- Don't mark input as dirty since changing model
        -- does not change the DOM input value.


-- This returns an greedy selection range for a new string based
-- on the selection range on the original string, using a diffing algo.
--
-- https://developer.mozilla.org/en-US/docs/Web/API/HTMLInputElement/setSelectionRange
-- selectionStart
-- The 0-based index of the first selected character.
-- selectionEnd
-- The 0-based index of the character after the last selected character.
--
-- So if there is no selection then selectionEnd == selectionStart
estimateSelectionRange :: String -> String -> Int -> Int -> (Int, Int)
estimateSelectionRange before after start end =
    let ds = D.getDiff before after
    in go ds start end 0 0
  where
    go :: [D.Diff Char] -> Int -> Int -> Int -> Int -> (Int, Int)
    go [] _ _ a b = (a, b)
    go (d : ds) s e a b =
        if (s <= 0 && e <= -1)
            then (a, b)
            else
                let (s', a') = step d s a
                    (e', b') = greedyStep d e b
                in go ds s' e' a' b'

    step :: D.Diff Char -> Int -> Int -> (Int, Int)
    step (D.First _) s s' = (if s > 0 then s - 1 else 0, s')
    step (D.Second _) s s' = (s, if s > 0 then s' + 1 else s')
    step (D.Both _ _) s s' = if s > 0
        then (s - 1, s' + 1)
        else (0, s')

    greedyStep :: D.Diff Char -> Int -> Int -> (Int, Int)
    greedyStep (D.First _) s s' = (if s >= 0 then s - 1 else (-1), s')
    greedyStep (D.Second _) s s' = (s, if s >= 0 then s' + 1 else s')
    greedyStep (D.Both _ _) s s' = if s > 0
        then (s - 1, s' + 1)
        else (-1, s')

----------------------------------------

-- | This provide a prototype of a checkbox input but without a builder.
-- Instead a lens to the CheckboxInput is used, and the user of this widget
-- is responsible for making the entire model.
checkboxInput :: ( Z.MonadReactor m)
    => Z.GadgetId
    -> Z.Prototype m v Bool (Which '[])
checkboxInput gid = Z.nulPrototype
    { Z.display = \s -> Z.lf' gid s "input"
        [ ("key", JE.toJSR . Z.reactKey $ s ^. Z.plan)
        , ("type", "checkbox")
        , ("checked", JE.toJSR $ s ^. Z.model)
        ]
    , Z.initializer = Z.withRef gid
        `Z.alsoInitializer` (Z.trigger' gid "onChange" () `Z.handledBy` hdlChange)
    }

  where
    hdlChange ::
        (Z.MonadReactor m)
        => Z.SceneHandler m v Bool a (Which '[])
    hdlChange this@(Z.Obj ref its) _ = Z.terminate' $ lift $ do
        Z.doModifyIORef' ref (its.Z.model %~ not)
        Z.dirty this

data IndeterminateCheckboxInput = IndeterminateCheckboxInput
    { checked :: Bool
    , indeterminate' :: Bool
    } deriving G.Generic

-- | This provide a prototype of a checkbox input but without a builder.
-- Instead a lens to the CheckboxInput is used, and the user of this widget
-- is responsible for making the entire model.
indeterminateCheckboxInput ::
    ( Z.MonadReactor m
    , Z.MonadJS m
    )
    => Z.GadgetId
    -> Z.Prototype m v IndeterminateCheckboxInput (Which '[])
indeterminateCheckboxInput gid = Z.magnifyPrototype (field @"checked") (checkboxInput gid)
    & Z.modifyInitializer fini
  where
    fini ini = ini `Z.alsoInitializer` onInitialized

    -- | Add setting the indeterminate' after every dirty as this is the only
    -- way to change that setting.
    onInitialized ::
        ( Z.MonadReactor m
        , Z.MonadJS m
        ) => Z.SceneInitializer m v IndeterminateCheckboxInput (Which '[])
    onInitialized this@(Z.Obj ref its) = Z.terminate' $ lift $ Z.addEveryOnUpdated this go
      where
        go = do
            obj <- Z.doReadIORef ref
            let j = obj ^. its.Z.plan.field @"refs".at gid
            case j of
                Nothing -> pure ()
                Just j' -> j' & Z.doSetProperty
                    ( "indeterminate'"
                    , JE.toJSR $ obj ^. its.Z.model.field @"indeterminate'"
                    )
