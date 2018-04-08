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

module Glazier.React.Widget.Input where --(
    -- -- * Text input
    -- textInput
    -- -- * Checkbox input
    -- , checkboxInput
    -- , IndeterminateCheckboxInput(..)
    -- , indeterminateCheckboxInput
    -- ) where

-- import Control.Applicative.Esoteric
import Control.Lens
import Control.Lens.Misc
import Control.Monad.Cont
import Control.Monad.Reader
import Control.Monad.Trans.Conts
import Control.Monad.Trans.Maybe
import qualified Data.Algorithm.Diff as D
import Data.Diverse.Lens
import qualified Data.DList as DL
import qualified Data.JSString as J
import Data.Maybe
import Data.Typeable
import qualified GHC.Generics as G
import Glazier.React
import Glazier.React.Effect.JavaScript
import Glazier.React.Framework
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
    ( Typeable p
    , AsFacet (TickState c) c
    , AsFacet (MkAction1 c) c
    , AsFacet (GetProperty c) c
    , AsFacet SetProperty c
    , AsFacet (MkAction c) c
    , AsFacet [c] c
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
        -- *> (trigger' gid "onChange" () >>= hdlChange)
    }
  where

    -- | Modify the DOM input value after every render to match the model value
    onInitialized ::
        ( Typeable p
        , AsFacet [c] c
        , AsFacet (TickState c) c
        , AsFacet (MkAction c) c
        , AsFacet (GetProperty c) c
        , AsFacet SetProperty c
        )
        => Gadget c p J.JSString ()
    onInitialized = do
        SceneObj _ _ slf <- ask
        triggerOnUpdated $ void $ runMaybeT $ do
            j <- MaybeT $ preuse (_scene._plan._gizmos.ix gid._targetRef._Just)
            s <- MaybeT $ preuse (_scene._model.slf)
            -- Use @('runCont` id)@ to allow do notation for making the continuation
            -- to put inside the commands.
            -- @runMaybeCmd@ adds 'MaybeT' to the 'Cont' stack.
            lift . post . (`runCont` id) . runMaybeCmd $ do
                start <- MaybeT . fmap JE.fromJSR . cont $ cmd' . GetProperty "selectionStart" j
                end <- MaybeT . fmap JE.fromJSR . cont $ cmd' . GetProperty "selectionEnd" j
                v <- MaybeT . fmap JE.fromJSR . cont $ cmd' . GetProperty "value" j
                let (a, b) = estimateSelectionRange (J.unpack v) (J.unpack s) start end
                pure $ cmd' @[]
                    [ cmd $ SetProperty ("value", JE.toJSR s) j
                    , cmd $ SetProperty ("selectionStart", JE.toJSR a) j
                    , cmd $ SetProperty ("selectionEnd", JE.toJSR b) j
                    ]

    -- hdlChange ::
    --     ( MonadReactor m, MonadJS m
    --     ) => a -> MethodT (Scene p m J.JSString) m ()
    -- hdlChange _ = readrT' $ \Obj{..} -> do
    --     lift $ void $ runMaybeT $ do
    --         me <- lift $ doReadIORef self
    --         j <- MaybeT $ pure $ me ^. my._plan._refs.at gid
    --         v <- lift $ (fromMaybe J.empty . JE.fromJSR) <$> (doGetProperty "value" j)
    --         lift $ doModifyIORef' self (my._model .~ v)
    --         -- Don't mark input as dirty since changing model
    --         -- does not change the DOM input value.


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

-- -- | This provide a prototype of a checkbox input but without a builder.
-- -- Instead a lens to the CheckboxInput is used, and the user of this widget
-- -- is responsible for making the entire model.
-- checkboxInput :: ( MonadReactor m)
--     => GadgetId
--     -> Prototype p Bool m ()
-- checkboxInput gid = mempty
--     { display = \s -> lf' gid s "input"
--         [ ("key", JE.toJSR . reactKey $ s ^. _plan)
--         , ("type", "checkbox")
--         , ("checked", JE.toJSR $ s ^. _model)
--         ]
--     , initializer = withRef gid
--         ^*> (trigger' gid "onChange" () >>= hdlChange)
--     }

--   where
--     hdlChange :: (MonadReactor m)
--         => a -> MethodT (Scene p m Bool) m ()
--     hdlChange _ = readrT' $ \this@Obj{..} -> do
--         lift $ do
--             doModifyIORef' self (my._model %~ not)
--             dirty this

-- data IndeterminateCheckboxInput = IndeterminateCheckboxInput
--     { checked :: Bool
--     , indeterminate :: Bool
--     } deriving G.Generic

-- makeLenses_ ''IndeterminateCheckboxInput

-- -- | This provide a prototype of a checkbox input but without a builder.
-- -- Instead a lens to the CheckboxInput is used, and the user of this widget
-- -- is responsible for making the entire model.
-- indeterminateCheckboxInput ::
--     ( MonadReactor m
--     , MonadJS m
--     )
--     => GadgetId
--     -> Prototype p IndeterminateCheckboxInput m ()
-- indeterminateCheckboxInput gid = magnifyPrototype _checked (checkboxInput gid)
--     & _initializer %~ (^*> onInitialized)
--   where
--     -- | Add setting the indeterminate after every dirty as this is the only
--     -- way to change that setting.
--     onInitialized ::
--         ( MonadReactor m
--         , MonadJS m
--         ) => MethodT (Scene p m IndeterminateCheckboxInput) m ()
--     onInitialized = readrT' $ \this@Obj{..} -> do
--          lift $ addEveryOnUpdated this (go this)
--       where
--         go Obj{..} = do
--             me <- doReadIORef self
--             let j = me ^. my._plan._refs.at gid
--             case j of
--                 Nothing -> pure ()
--                 Just j' -> j' & doSetProperty
--                     ( "indeterminate"
--                     , JE.toJSR $ me ^. my._model._indeterminate
--                     )
