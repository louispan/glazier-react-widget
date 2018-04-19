{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Glazier.React.Effect.HTMLElement where

import Control.Lens
import Control.Monad.State
import Data.Diverse.Lens
import Glazier.React

-- Effects from methods in https://developeR.mozilla.org/en-US/docs/Web/API/HTMLElement
data HTMLElementCmd =  Focus EventTarget | Blur EventTarget
    deriving Show

focusRef ::
    ( MonadState (Scenario c s) m
    , AsFacet HTMLElementCmd c
    )
    => GizmoId -> m ()
focusRef gid = do
    t <- preuse (_scene._plan._gizmos.ix gid._targetRef._Just)
    case t of
        Nothing -> pure ()
        Just t' -> post . cmd $ Focus t'

blurRef ::
    ( MonadState (Scenario c s) m
    , AsFacet HTMLElementCmd c
    )
    => GizmoId -> m ()
blurRef gid = do
    t <- preuse (_scene._plan._gizmos.ix gid._targetRef._Just)
    case t of
        Nothing -> pure ()
        Just t' -> post . cmd $ Blur t'
