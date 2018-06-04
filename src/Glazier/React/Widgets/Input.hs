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
    ( OnChangeInput(..)
    -- * Text input
    , textInput
    -- * Checkbox input
    , checkboxInput
    , IndeterminateCheckboxInput(..)
    , indeterminateCheckboxInput
    ) where

import Control.Lens
import Control.Lens.Misc
import qualified Data.Algorithm.Diff as D
import qualified Data.JSString as J
import qualified GHC.Generics as G
import Glazier.React
import Glazier.React.Effect.JavaScript
import qualified JavaScript.Extras as JE

----------------------------------------

data OnChangeInput = OnChangeInput

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
    ( AsReactor cmd
    , AsJavascript cmd
    )
    => ReactId -> Widget cmd p J.JSString OnChangeInput
textInput ri =
    let win = do
            s <- ask
            lf' ri "input"
                [ ("key", JE.toJSR ri)
                -- "value" cannot be used as React will take over as a controlled component.
                -- The defaultValue only sets the *initial* DOM value
                -- The user will need to modify reactKey if they want
                -- react to actually rerender, since React will not do anything
                -- even if defaultValue changes.
                -- But hopefully this is not necessary as the DOM inpt value
                -- is updated under the hood in onInitialized
                , ("defaultValue", JE.toJSR $ s ^. _model)
                ]
        gad = finish (hdlElementalRef ri)
            `also` finish (hdlRendered)
            `also` hdlChange
    in (display win) `also` (lift gad)
  where

    -- | Modify the DOM input value after every render to match the model value
    hdlRendered ::
        ( AsReactor cmd
        , AsJavascript cmd
        )
        => Gadget cmd p J.JSString ()
    hdlRendered = onRendered _always $ do
        scn <- getScene
        void $ runMaybeT $ do
            j <- MaybeT $ pure $ preview (elementTarget ri) scn
            let s = view _model scn
            start <- MaybeT . fmap JE.fromJSR . sequel $ postCmd' . GetProperty j "selectionStart"
            end <- MaybeT . fmap JE.fromJSR . sequel $ postCmd' . GetProperty j "selectionEnd"
            v <- MaybeT . fmap JE.fromJSR . sequel $ postCmd' . GetProperty j "value"
            let (a, b) = estimateSelectionRange (J.unpack v) (J.unpack s) start end
            postCmd' $ SetProperty j ("value", JE.toJSR s)
            postCmd' $ SetProperty j ("selectionStart", JE.toJSR a)
            postCmd' $ SetProperty j ("selectionEnd", JE.toJSR b)

    hdlChange ::
        ( AsReactor cmd
        , AsJavascript cmd
        )
        => Gadget cmd p J.JSString OnChangeInput
    hdlChange = do
        trigger_ ri _always "onChange" ()
        scn <- getScene
        maybeDelegate () $ runMaybeT $ do
            j <- MaybeT $ pure $ preview (elementTarget ri) scn
            v <- MaybeT . fmap JE.fromJSR . sequel $ postCmd' . GetProperty j "value"
            tickScene $ _model .= v
            pure OnChangeInput

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
    AsReactor cmd
    => ReactId -> Widget cmd p Bool OnChangeInput
checkboxInput ri =
    let win = do
            s <- ask
            lf' ri "input"
                [ ("key", JE.toJSR ri)
                , ("type", "checkbox")
                , ("checked", JE.toJSR $ s ^. _model)
                ]
        gad = (finish (hdlElementalRef ri))
            `also` hdlChange
    in (display win) `also` (lift gad)
  where
    hdlChange ::
        AsReactor cmd
        => Gadget cmd p Bool OnChangeInput
    hdlChange = do
        trigger_ ri _always "onChange" ()
        tickScene $ _model %= not
        pure OnChangeInput

data IndeterminateCheckboxInput = IndeterminateCheckboxInput
    { checked :: Bool
    , indeterminate :: Bool
    } deriving (G.Generic, Show, Eq, Ord)

makeLenses_ ''IndeterminateCheckboxInput

-- | Variation of 'checkboxInput' supporting indeterminate state.
indeterminateCheckboxInput ::
    ( AsReactor cmd
    , AsJavascript cmd
    )
    => ReactId -> Widget cmd p IndeterminateCheckboxInput OnChangeInput
indeterminateCheckboxInput ri = magnifyWidget _checked (checkboxInput ri)
    `also` finish (lift hdlRendered)
  where
    hdlRendered ::
        ( AsReactor cmd
        , AsJavascript cmd
        )
        => Gadget cmd p IndeterminateCheckboxInput ()
    hdlRendered = onRendered _always $ do
        scn <- getScene
        void $ runMaybeT $ do
            j <- MaybeT $ pure $ preview (elementTarget ri) scn
            i <- MaybeT $ pure $ preview (_model._indeterminate) scn
            postCmd' $ SetProperty j ("indeterminate", JE.toJSR i)
