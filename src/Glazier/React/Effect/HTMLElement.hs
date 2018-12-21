{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Glazier.React.Effect.HTMLElement where

import Data.Diverse.Lens
import Glazier.React
import GHC.Stack

type AsHTMLElement c = AsFacet HTMLElementCmd c

-- Effects from methods in https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement
data HTMLElementCmd = Focus EventTarget | Blur EventTarget
    deriving Show

focus ::
    ( HasCallStack
    , AsReactor c
    , AsHTMLElement c
    , MonadCommand c m
    )
    => EventTarget -> m ()
focus j = tracedExec callStack $ Focus j

blur ::
    ( HasCallStack
    , AsReactor c
    , AsHTMLElement c
    , MonadCommand c m
    )
    => EventTarget -> m ()
blur j = tracedExec callStack $ Blur j
