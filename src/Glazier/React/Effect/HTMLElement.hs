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
    , AsHTMLElement c
    , AsFacet LogLine c
    , MonadProgram c m
    , AskLogLevel m
    )
    => EventTarget -> m ()
focus j = logExec TRACE callStack $ Focus j

blur ::
    ( HasCallStack
    , AsHTMLElement c
    , AsFacet LogLine c
    , MonadProgram c m
    , AskLogLevel m
    )
    => EventTarget -> m ()
blur j = logExec TRACE callStack $ Blur j
