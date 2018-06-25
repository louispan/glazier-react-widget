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

-- focus ::
--     ( AsHTMLElement cmd
--     , MonadCommand cmd m
--     )
--     => EventTarget -> m ()
-- focus = postCmd . Focus

-- blur ::
--     ( AsHTMLElement cmd
--     , MonadCommand cmd m
--     )
--     => EventTarget -> m ()
-- blur = postCmd . Blur
