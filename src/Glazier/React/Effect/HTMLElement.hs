{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Glazier.React.Effect.HTMLElement where

import Data.Diverse.Lens
import Glazier.React

type AsHTMLElement c = AsFacet HTMLElementCmd c

-- Effects from methods in https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement
data HTMLElementCmd = Focus EventTarget | Blur EventTarget
    deriving Show

focus ::
    ( HasCallStack
    , MonadReactor c m
    , AsHTMLElement c
    )
    => EventTarget -> m ()
focus j = tracedExec callStack $ Focus j

blur ::
    ( HasCallStack
    , MonadReactor c m
    , AsHTMLElement c
    )
    => EventTarget -> m ()
blur j = tracedExec callStack $ Blur j
