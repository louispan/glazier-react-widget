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
import Glazier.DOM as DOM
import Glazier.React
import Glazier.React.Widgets.Input.Internal

----------------------------------------

-- data InputChange = InputChange

-- Tagged event. The convention is to fire "OnXXX" if the event is not handled
-- or fire "XXX" to notify handled events.
-- type InputChange = Tagged "InputChange"

-- | Text inputs dosn't interact well as a React controlled component.
-- Eg. cursor jumps if user types quickly.
--
-- It is because there is a race condition with lazy event handlers setting the value,
-- So this prototype uses the React uncontrolled component
-- (using defaultValue instead of value).
--
-- For <input>, React uses controlled input if input.value is not null
-- and there is an onChange handler.
--
-- This widget attempts to set the cursor position at the correct place
-- by using a diffing algorithm on the old and new value.
--
-- Warning: This widget listens to onChange and will update the model value with the DOM input value.
-- potentially overridding any user changes to the model
-- So when changing the model value, be sure that the onChange handler will not be called.


input :: (MonadWidget s c m, MonadObserver (Tagged "InputChange" ()) m)
    => Lens' s JSString
    -> DL.DList (JSString, m Handler)
    -> DL.DList (JSString, Getting' s JSVal)
    -> m ()
input this gads props = lf inputComponent
    ([("onChange", onChange)] <> gads)
    ([("value", this.toJS_)] <> props)
  where
    onChange = mkSyntheticHandler fromChange hdlChange
    fromChange = maybeM . fmap fromJS . (`getProperty` "value") . target
    hdlChange v = do
        mutate RerenderNotRequired $ this .= v
        observe $ Tagged @"InputChange" ()

----------------------------------------

-- For an indeterminate checkbox, just set the property "indeterminate" to true.
checkbox :: (MonadWidget s c m, MonadObserver (Tagged "InputChange" ()) m)
    => Lens' s Bool
    -> DL.DList (JSString, m Handler)
    -> DL.DList (JSString, Getting' s JSVal)
    -> m ()
checkbox this gads props = lf inputComponent
    ([("onChange", onChange)] <> gads)
    ([("type", strProp "checkbox"), ("checked", this.toJS_)] <> props)
  where
    onChange = mkSyntheticHandler (const $ pure ()) hdlChange
    hdlChange () = do
        mutate RerenderNotRequired $ this %= not
        observe $ Tagged @"InputChange" ()
