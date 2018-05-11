{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Glazier.React.Effect.HTMLElement where

import Control.Lens
import Data.Diverse.Lens
import Glazier.React

type AsHTMLElement c = AsFacet HTMLElementCmd c

-- Effects from methods in https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement
data HTMLElementCmd =  Focus EventTarget | Blur EventTarget
    deriving Show

focusElement ::
    ( MonadScenario s cmd m
    , AsHTMLElement cmd
    )
    => ElementalId -> m ()
focusElement eid = do
    t <- preview (_plan._elementals.ix eid._elementalRef._Just)
    case t of
        Nothing -> pure ()
        Just t' -> mandate $ Focus t'

blurElement ::
    ( MonadScenario s cmd m
    , AsHTMLElement cmd
    )
    => ElementalId -> m ()
blurElement eid = do
    t <- preview (_plan._elementals.ix eid._elementalRef._Just)
    case t of
        Nothing -> pure ()
        Just t' -> mandate $ Blur t'
