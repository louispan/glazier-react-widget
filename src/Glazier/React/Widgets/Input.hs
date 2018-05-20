{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Widgets.Input
    (
    -- * Text input
    textInput
    -- * Checkbox input
    , checkboxInput
    , IndeterminateCheckboxInput(..)
    , indeterminateCheckboxInput
    ) where

import Control.Lens
import Control.Lens.Misc
import Control.Monad.Cont
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import qualified Data.Algorithm.Diff as D
import qualified Data.JSString as J
import Data.Semigroup
import qualified GHC.Generics as G
import Glazier.React
import Glazier.React.Effect.JavaScript
import qualified JavaScript.Extras as JE

----------------------------------------

-- | Text inputs dosn't interact well as a React controlled component.
-- Eg. cursor jumps if user types quickly.
-- I think there a timing issue with lazy event handlers setting the value,
-- So this prototype uses the React uncontrolled component
-- (using defaultValue instead of value).
--
-- For input, React uses controlled input if input.value is not null.
--
-- This widget attempts to set the cursor position at the correct place
-- by using a diffing algorithm on the old and new value.
--
-- Warning: This widget listens to onChange and will update the model value with the DOM input value.
-- potentially overridding any user changes.
-- So when changing the model value, be sure that the onChange handler will not be called.
textInput ::
    ( AsReactor c
    , AsJavascript c
    )
    => ElementalId
    -> Widget c p J.JSString ()
textInput eid = dummy
    { window = do
        s <- ask
        lf' eid "input"
            [ ("key", JE.toJSR eid)
            -- "value" cannot be used as React will take over as a controlled component.
            -- The defaultValue only sets the *initial* DOM value
            -- The user will need to modify reactKey if they want
            -- react to actually rerender, since React will not do anything
            -- even if defaultValue changes.
            -- But hopefully this is not necessary as the DOM inpt value
            -- is updated under the hood in onInitialized
            , ("defaultValue", JE.toJSR $ s ^. _model)
            ]
    , gadget = hdlElementalRef eid
        <> hdlRendered
        <> hdlChange
    }
  where

    -- | Modify the DOM input value after every render to match the model value
    hdlRendered ::
        ( AsReactor c
        , AsJavascript c
        )
        => Gadget c p J.JSString ()
    hdlRendered = onRendered _always $ getScene $ \scn -> void $ runMaybeT $ do
            j <- MaybeT $ pure $ preview (elementTarget eid) scn
            let s = view _model scn
            start <- MaybeT . fmap JE.fromJSR . conclude $ postcmd' . GetProperty j "selectionStart"
            end <- MaybeT . fmap JE.fromJSR . conclude $ postcmd' . GetProperty j "selectionEnd"
            v <- MaybeT . fmap JE.fromJSR . conclude $ postcmd' . GetProperty j "value"
            let (a, b) = estimateSelectionRange (J.unpack v) (J.unpack s) start end
            postcmd' $ SetProperty j ("value", JE.toJSR s)
            postcmd' $ SetProperty j ("selectionStart", JE.toJSR a)
            postcmd' $ SetProperty j ("selectionEnd", JE.toJSR b)

    hdlChange ::
        ( AsReactor c
        , AsJavascript c
        )
        => Gadget c p J.JSString ()
    hdlChange = do
        trigger' _always eid "onChange" (const $ pure ())
        getScene $ \scn -> void $ runMaybeT $ do
            j <- MaybeT $ pure $ preview (elementTarget eid) scn
            v <- MaybeT . fmap JE.fromJSR . conclude $ postcmd' . GetProperty j "value"
            tickScene $ _model .= v
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

-- | This is a 'React controlled' checkbox.
-- For checkboxes,  React uses controlled checkbox if input.checked is not null
-- https://stackoverflow.com/questions/37427508/react-changing-an-uncontrolled-input
checkboxInput ::
    AsReactor c
    => ElementalId
    -> Widget c p Bool ()
checkboxInput eid = dummy
    { window = do
        s <- ask
        lf' eid "input"
            [ ("key", JE.toJSR eid)
            , ("type", "checkbox")
            , ("checked", JE.toJSR $ s ^. _model)
            ]
    , gadget = hdlElementalRef eid
        <> hdlChange
    }

  where
    hdlChange ::
        AsReactor c
        => Gadget c p Bool ()
    hdlChange = do
        trigger' _always eid "onChange" (const $ pure ())
        tickScene $ _model %= not

data IndeterminateCheckboxInput = IndeterminateCheckboxInput
    { checked :: Bool
    , indeterminate :: Bool
    } deriving (G.Generic, Show, Eq, Ord)

makeLenses_ ''IndeterminateCheckboxInput

-- | Variation of 'checkboxInput' supporting indeterminate state.
indeterminateCheckboxInput ::
    ( AsReactor c
    , AsJavascript c
    )
    => ElementalId
    -> Widget c p IndeterminateCheckboxInput ()
indeterminateCheckboxInput eid = enlargeModel _checked (checkboxInput eid)
    & _gadget %~ (<> hdlRendered)
  where
    hdlRendered ::
        ( AsReactor c
        , AsJavascript c
        )
        => Gadget c p IndeterminateCheckboxInput ()
    hdlRendered = onRendered _always $ getScene $ \scn -> void $ runMaybeT $ do
            j <- MaybeT $ pure $ preview (elementTarget eid) scn
            i <- MaybeT $ pure $ preview (_model._indeterminate) scn
            postcmd' $ SetProperty j ("indeterminate", JE.toJSR i)
