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
    ( AsHTMLElement cmd
    , MonadReactor p s cmd m
    )
    => ElementalId -> m ()
focusElement eid = getScene $ \scn -> do
    let t = preview (_plan._elementals.ix eid._elementalRef._Just) scn
    case t of
        Nothing -> pure ()
        Just t' -> postCmd $ Focus t'

blurElement ::
    ( AsHTMLElement cmd
    , MonadReactor p s cmd m
    )
    => ElementalId -> m ()
blurElement eid = getScene $ \scn -> do
    let t = preview (_plan._elementals.ix eid._elementalRef._Just) scn
    case t of
        Nothing -> pure ()
        Just t' -> postCmd $ Blur t'
