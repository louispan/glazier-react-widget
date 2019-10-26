{-# OPTIONS_GHC -Wno-type-defaults #-}

{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Glazier.React.Widgets.Input
( input
, checkbox
, inputComponent
) where

-- import qualified Data.Aeson as A
-- import qualified Data.Aeson.Applicative as A
import qualified Data.DList as DL
import qualified Glazier.DOM as DOM
import Glazier.React
import Glazier.React.Widgets.Input.Internal

default (JSString)

----------------------------------------

-- In GHJS, text inputs doesn't interact well as a React controlled component.
-- Eg. cursor jumps if user types quickly.

-- It is because there is a race condition between user updating the DOM input asynchronously
-- vs lazy event handlers saving the DOM value into the widget.
-- If saving the DOM value into the widget triggers a render, it could rerender
-- be a "stale" value that is behind the current DOM value typed by the user.
--
-- This widget uses the InputComponent wrapper in
-- glazier-react-widget.js which uses a React uncontrolled component
-- and does not trigger a rerender when mutating the model, since the
-- DOM input do not need to be rerendered with user input.
--
-- The DOM input value is considered a source of truth, while the widget saved value
-- is a potentially stale copy.
input :: (MonadWidget m, MonadModel s m)
    => Traversal' s JSString
    -> DL.DList (JSString, Prop s m Handler)
    -> DL.DList (JSString, Prop s m (Maybe JSVal))
    -> m ()

input this gads props = do
    s <- askModel
    when (has this s) $
        lf inputComponent
            ([("onChange", onChange)] <> gads)
            ([("value", premodel $ this._toJS)] <> props)
  where
    onChange = mkHandler' fromChange handlChange
    fromChange = fromJustIO . fmap fromJS . (`getProperty` "value") . DOM.target
    handlChange v = quietMutate $ this .= v

----------------------------------------

-- | This widget uses the InputComponent wrapper in
-- glazier-react-widget.js which allows setting the property "indeterminate" to
-- render an intermediate checkbox.
checkbox :: (MonadWidget m, MonadModel s m)
    => Traversal' s Bool
    -> DL.DList (JSString, Prop s m Handler)
    -> DL.DList (JSString, Prop s m (Maybe JSVal))
    -> m ()
checkbox this gads props = do
    s <- askModel
    when (has this s) $
        lf inputComponent
        ([("onChange", onChange)] <> gads)
        ([("type", "checkbox"), ("checked", premodel $ this._toJS)] <> props)
  where
    onChange = mkHandler' fromChange handlChange
    fromChange = fromJustIO . fmap fromJS . (`getProperty` "checked") . DOM.target
    handlChange v = quietMutate $ this .= v
