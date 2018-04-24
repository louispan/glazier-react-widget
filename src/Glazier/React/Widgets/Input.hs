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

{-# LANGUAGE NoMonomorphismRestriction #-}

module Glazier.React.Widgets.Input
    (
    -- * Text input
    textInput
    -- * Checkbox input
    , checkboxInput
    , IndeterminateCheckboxInput(..)
    , indeterminateCheckboxInput
    ) where

-- import Control.Applicative.Esoteric
import Control.Lens
import Control.Lens.Misc
import Control.Monad.Cont
import Control.Monad.Reader
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Maybe
import qualified Data.Algorithm.Diff as D
import qualified Data.JSString as J
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
    ( AsReactor c
    , AsJavascript c
    )
    => GizmoId
    -> Widget c p J.JSString ()
textInput gid = dummy
    { window = do
        s <- ask
        lf' gid "input"
            [ ("key", JE.toJSR gid)
            -- "value" cannot be used as React will take over as a controlled component.
            -- The defaultValue only sets the *initial* DOM value
            -- The user will need to modify reactKey if they want
            -- react to actually rerender, since React will not do anything
            -- even if defaultValue changes.
            -- But hopefully this is not necessary as the DOM inpt value
            -- is updated under the hood in onInitialized
            , ("defaultValue", JE.toJSR $ s ^. _model)
            ]
    , gadget = withRef gid
        *> onInitialized
        *> (trigger gid "onChange" (pure ()) >>= hdlChange)
    }
  where

    -- | Modify the DOM input value after every render to match the model value
    onInitialized ::
        ( AsReactor c
        , AsJavascript c
        )
        => Gadget c p J.JSString ()
    onInitialized = do
        Traversal slf <- view _self
        triggerOnUpdated $ void $ runMaybeT $ do
            j <- MaybeT . preuse $ _scene._plan._gizmos.ix gid._targetRef._Just
            s <- MaybeT . preuse $ _scene._model.slf
            -- Use @('runCont` id)@ to allow do notation for making the continuation
            -- to put inside the commands.
            -- @runMaybeCmd@ adds 'MaybeT' to the 'Cont' stack.
            postState . evalContT . void . runMaybeT $ do
                start <- MaybeT . fmap JE.fromJSR . retrieve $ cmd' . GetProperty j "selectionStart"
                end <- MaybeT . fmap JE.fromJSR . retrieve $ cmd' . GetProperty j "selectionEnd"
                v <- MaybeT . fmap JE.fromJSR . retrieve $ cmd' . GetProperty j "value"
                let (a, b) = estimateSelectionRange (J.unpack v) (J.unpack s) start end
                post' $ cmd' @[]
                    [ cmd' $ SetProperty j ("value", JE.toJSR s)
                    , cmd' $ SetProperty j ("selectionStart", JE.toJSR a)
                    , cmd' $ SetProperty j ("selectionEnd", JE.toJSR b)
                    ]

    hdlChange ::
        ( AsReactor c
        , AsJavascript c
        )
        => a -> Gadget c p J.JSString ()
    hdlChange _ = do
        Traversal slf <- view _self
        sbj <- view _subject
        void $ runMaybeT $ do
            j <- MaybeT $ preuse (_scene._plan._gizmos.ix gid._targetRef._Just)
            postState . evalContT . void . runMaybeT $ do
                v <- MaybeT . fmap JE.fromJSR . retrieve $ cmd' . GetProperty j "value"
                post' $ cmd' $ TickState sbj (_scene._model.slf .= v)
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
checkboxInput ::
    AsReactor c
    => GizmoId
    -> Widget c p Bool ()
checkboxInput gid = dummy
    { window = do
        s <- ask
        lf' gid "input"
            [ ("key", JE.toJSR gid)
            , ("type", "checkbox")
            , ("checked", JE.toJSR $ s ^. _model)
            ]
    , gadget = withRef gid
        *> (trigger gid "onChange" (pure ()) >>= hdlChange)
    }

  where
    hdlChange ::
        AsReactor c
        => a -> Gadget c p Bool ()
    hdlChange _ = do
        Traversal slf <- view _self
        sbj <- view _subject
        post . cmd' $ TickState sbj $ do
            _scene._model.slf %= not
            dirty

data IndeterminateCheckboxInput = IndeterminateCheckboxInput
    { checked :: Bool
    , indeterminate :: Bool
    } deriving G.Generic

makeLenses_ ''IndeterminateCheckboxInput

-- | This provide a prototype of a checkbox input but without a builder.
-- Instead a lens to the CheckboxInput is used, and the user of this widget
-- is responsible for making the entire model.
indeterminateCheckboxInput ::
    ( AsReactor c
    , AsJavascript c
    )
    => GizmoId
    -> Widget c p IndeterminateCheckboxInput ()
indeterminateCheckboxInput gid = enlargeModel _checked (checkboxInput gid)
    & _gadget %~ (*> onInitialized)
  where
    onInitialized ::
        ( AsReactor c
        , AsJavascript c
        )
        => Gadget c p IndeterminateCheckboxInput ()
    onInitialized = do
        Traversal slf <- view _self
        triggerOnUpdated $ void $ runMaybeT $ do
            j <- MaybeT . preuse $ _scene._plan._gizmos.ix gid._targetRef._Just
            i <- MaybeT . preuse $ _scene._model.slf._indeterminate
            post . cmd' $ SetProperty j ("indeterminate", JE.toJSR i)
