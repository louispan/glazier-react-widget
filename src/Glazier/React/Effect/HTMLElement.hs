{-# LANGUAGE FlexibleContexts #-}

module Glazier.React.Effect.HTMLElement where

import Control.Lens
import Control.Monad.State
import Data.Diverse.Lens
import Glazier.React
import Glazier.React.Framework

-- Effects from methods in https://developeR.mozilla.org/en-US/docs/Web/API/HTMLElement
data Focus = Focus EventTarget

focusRef ::
    ( HasPlan s
    , HasCommands c s
    , MonadState s m
    , AsFacet Focus c
    )
    => GizmoId -> m ()
focusRef gid = do
    t <- preuse (_plan._gizmos.ix gid._targetRef._Just)
    case t of
        Nothing -> pure ()
        Just t' -> post1 (Focus t')

data Blur = Blur EventTarget

blurRef ::
    ( HasPlan s
    , HasCommands c s
    , MonadState s m
    , AsFacet Blur c
    )
    => GizmoId -> m ()
blurRef gid = do
    t <- preuse (_plan._gizmos.ix gid._targetRef._Just)
    case t of
        Nothing -> pure ()
        Just t' -> post1 (Blur t')
