{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Widgets.Input
    ( -- * Text input
    textInput
    -- * Checkbox input
    , checkboxInput
    , IndeterminateCheckboxInput(..)
    , indeterminateCheckboxInput
    ) where

import Control.Lens
import Control.Lens.Misc
import qualified Data.Algorithm.Diff as D
import qualified Data.JSString as J
import Data.Tagged
import qualified GHC.Generics as G
import Glazier.React
import Glazier.React.Effect.JavaScript
import Glazier.React.Event.Synthetic
import qualified JavaScript.Extras as JE

----------------------------------------

-- Tagged event. The convention is to fire "OnXXX" if the event is not handled
-- or fire "XXX" to notify handled events.
type InputChange = Tagged "InputChange"

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
    => ReactId -> Widget cmd p J.JSString (InputChange ())
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
        gad = (finish hdlRendered)
            `also` hdlChange
    in (display win) `also` (lift gad)
  where

    -- | Modify the DOM input value after every render to match the model value
    hdlRendered ::
        ( AsReactor cmd
        , AsJavascript cmd
        )
        => Gadget cmd p J.JSString ()
    hdlRendered = onRendered $ do
        s <- getModel
        j <- getElementalRef ri
        (`evalMaybeT` ()) $ do
            start <- maybeGetProperty "selectionStart" j
            end <- maybeGetProperty "selectionEnd" j
            v <- maybeGetProperty "value" j
            let (a, b) = estimateSelectionRange (J.unpack v) (J.unpack s) start end
            exec' $ SetProperty ("value", JE.toJSR s) j
            exec' $ SetProperty ("selectionStart", JE.toJSR a) j
            exec' $ SetProperty ("selectionEnd", JE.toJSR b) j

    hdlChange ::
        ( AsReactor cmd
        , AsJavascript cmd
        )
        => Gadget cmd p J.JSString (InputChange ())
    hdlChange = do
        j <- trigger ri "onChange" (pure . target . toSyntheticEvent)
        maybeDelegate () $ runMaybeT $ do
            v <- maybeGetProperty "value" j
            tickModel $ id .= v
            pure $ Tagged @"InputChange" ()

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
    (AsReactor cmd)
    => ReactId -> Widget cmd p Bool (InputChange ())
checkboxInput ri =
    let win = do
            s <- ask
            lf' ri "input"
                [ ("key", JE.toJSR ri)
                , ("type", "checkbox")
                , ("checked", JE.toJSR $ s ^. _model)
                ]
        gad = hdlChange
    in (display win) `also` (lift gad)
  where
    hdlChange ::
        (AsReactor cmd)
        => Gadget cmd p Bool (InputChange ())
    hdlChange = do
        trigger_ ri "onChange" ()
        tickModel $ id %= not
        pure $ Tagged @"InputChange" ()

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
    => ReactId -> Widget cmd p IndeterminateCheckboxInput (InputChange ())
indeterminateCheckboxInput ri = magnifyWidget _checked (checkboxInput ri)
    `also` finish (lift hdlRendered)
  where
    hdlRendered ::
        ( AsReactor cmd
        , AsJavascript cmd
        )
        => Gadget cmd p IndeterminateCheckboxInput ()
    hdlRendered = onRendered $ do
        j <- getElementalRef ri
        s <- getModel
        (`evalMaybeT` ()) $ do
            i <- MaybeT $ pure $ preview _indeterminate s
            exec' $ SetProperty ("indeterminate", JE.toJSR i) j
