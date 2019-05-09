{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Widgets.Input where

import qualified Data.Aeson as A
import qualified Data.Aeson.Applicative as A
import qualified Data.Algorithm.Diff as D
import qualified Data.DList as DL
import qualified Data.JSString as J
import qualified GHC.Generics as G
import Glazier.React
import Glazier.React.Effect.JavaScript
import Glazier.React.Event.Synthetic

----------------------------------------

-- data InputChange = InputChange

-- Tagged event. The convention is to fire "OnXXX" if the event is not handled
-- or fire "XXX" to notify handled events.
-- type InputChange = Tagged "InputChange"

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
    ( HasCallStack
    , AsReactor c
    , AsJavascript c
    , MonadWidget c s m
    , Observer (Tagged "InputChange" ()) m
    )
    => Traversal' s J.JSString
    -> DL.DList (J.JSString, Prop s)
    -> DL.DList (Gadget m ())
    -> Widget m ()
textInput this props gads = lf "input"
    -- "value" cannot be used as React will take over as a controlled component.
    -- The defaultValue only sets the *initial* DOM value
    -- The user will need to modify reactKey if they want
    -- react to actually rerender, since React will not do anything
    -- even if defaultValue changes.
    -- But hopefully this is not necessary as the DOM input value
    -- is updated under the hood in onRendered
    ([("default", propM $ preview this)] <> props)
    ([hdlRendered, hdlChange] <> gads)
  where
    -- | Modify the DOM input value after every render to match the model value
    hdlRendered = onRendered $ do
        s <- getModel this
        j <- getReactRef -- "input" element
        start <- fromProperty "selectionStart" j
        end <- fromProperty "selectionEnd" j
        v <- fromProperty "value" j
        let (a, b) = estimateSelectionRange (J.unpack v) (J.unpack s) start end
        setProperty ("value", s) j
        setProperty ("selectionStart", a) j
        setProperty ("selectionEnd", b) j

    hdlChange = trigger "onChange" (pure . target . toSyntheticEvent) $ \j -> do
        v <- fromProperty "value" j
        mutate $ this .= v
        observe $ Tagged @"InputChange" ()

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
    ( HasCallStack
    , AsReactor c
    , MonadWidget c s m
    , Observer (Tagged "InputChange" ()) m
    )
    => Traversal' s Bool
    -> DL.DList (J.JSString, Prop s)
    -> DL.DList (Gadget m ())
    -> Widget m ()
checkboxInput this props gads = lf "input"
    ([ ("type", strProp "checkbox")
    , ("checked", propM $ preview this)
    ] <> props)
    ([hdlChange] <> gads)
  where
    hdlChange = trigger_ "onChange" () . const $ do
        mutate $ this %= not
        observe $ Tagged @"InputChange" ()

data IndeterminateCheckboxInput = IndeterminateCheckboxInput
    { checked :: Bool
    , indeterminate :: Bool
    } deriving (G.Generic, Show, Eq, Ord)

makeLenses_ ''IndeterminateCheckboxInput

instance A.ToJSON IndeterminateCheckboxInput where toEncoding = A.genericToEncoding A.defaultOptions
instance Applicative m => A.AToJSON m IndeterminateCheckboxInput

-- instance A.FromJSON IndeterminateCheckboxInput
-- instance Applicative m => A.AFromJSON m IndeterminateCheckboxInput

-- | Variation of 'checkboxInput' supporting indeterminate state.
indeterminateCheckboxInput ::
    ( HasCallStack
    , AsReactor c
    , AsJavascript c
    , MonadWidget c s m
    , Observer (Tagged "InputChange" ()) m
    )
    => Traversal' s IndeterminateCheckboxInput
    -> DL.DList (J.JSString, Prop s)
    -> DL.DList (Gadget m ())
    -> Widget m ()
indeterminateCheckboxInput this props gads =
    checkboxInput (this._checked)
    props
    (hdlRendered `DL.cons` gads)
  where
    hdlRendered = onRendered $ do
        j <- getReactRef
        s <- getModel this
        i <- whenJust $ preview _indeterminate s
        setProperty ("indeterminate", i) j
