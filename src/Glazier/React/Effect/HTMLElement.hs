{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Glazier.React.Effect.HTMLElement where

import Data.Diverse.Lens
import Glazier.React

type AsHTMLElement cmd = AsFacet HTMLElementCmd cmd

-- Effects from methods in https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement
data HTMLElementCmd = Focus EventTarget | Blur EventTarget
    deriving Show

focusElement ::
    ( AsHTMLElement cmd
    , MonadReactor p s cmd m
    )
    => ReactId -> m ()
focusElement ri = do
    j <- getElementalRef ri
    postCmd $ Focus j

blurElement ::
    ( AsHTMLElement cmd
    , MonadReactor p s cmd m
    )
    => ReactId -> m ()
blurElement ri = do
    j <- getElementalRef ri
    postCmd $ Blur j
