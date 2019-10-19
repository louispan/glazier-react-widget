{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Widgets.Input where

-- import qualified Data.Aeson as A
-- import qualified Data.Aeson.Applicative as A
import qualified Data.DList as DL
import qualified Glazier.DOM as DOM
import Glazier.React
import Glazier.React.Widgets.Input.Internal

----------------------------------------

-- In GHJS, text inputs doesn't interact well as a React controlled component.
-- Eg. cursor jumps if user types quickly.

-- It is because there is a race condition with lazy event handlers setting the value,
-- So this prototype uses the React uncontrolled component
-- (using defaultValue instead of value).
--
-- Therefore this widget uses the InputComponent wrapper in
-- glazier-react-widget.js which uses a React uncontrolled component
-- and does not trigger a rerender when mutating the model (since the
-- DOM input do not need to be rerendered with user input.
input :: (MonadWidget s m, MonadObserver' a m)
    => a
    -> Traversal' s JSString
    -> DL.DList (JSString, m Handler)
    -> DL.DList (JSString, Prop s)
    -> m ()
input a this gads props = do
    s <- askModel
    when (has this s) $
        lf inputComponent
            ([("onChange", onChange)] <> gads)
            ([("value", preview $ this._toJS)] <> props)
  where
    onChange = mkSyntheticHandler fromChange hdlChange
    fromChange = maybeM . fmap fromJS . (`getProperty` "value") . DOM.target
    hdlChange v = do
        mutate RerenderNotRequired $ this .= v
        observe' a

----------------------------------------

-- | This widget uses the InputComponent wrapper in
-- glazier-react-widget.js which allows setting the property "indeterminate" to
-- render an intermediate checkbox.
checkbox :: (MonadWidget s m, MonadObserver' a m)
    => a
    -> Traversal' s Bool
    -> DL.DList (JSString, m Handler)
    -> DL.DList (JSString, Prop s)
    -> m ()
checkbox a this gads props = do
    s <- askModel
    when (has this s) $
        lf inputComponent
        ([("onChange", onChange)] <> gads)
        ([("type", strProp "checkbox"), ("checked", preview $ this._toJS)] <> props)
  where
    onChange = mkSyntheticHandler fromChange hdlChange
    fromChange = maybeM . fmap fromJS . (`getProperty` "checked") . DOM.target
    hdlChange v = do
        mutate RerenderNotRequired $ this .= v
        observe' a
