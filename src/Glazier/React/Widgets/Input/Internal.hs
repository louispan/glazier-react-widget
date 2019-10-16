{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Glazier.React.Widgets.Input.Internal
    ( InputComponent(..)
    , inputComponent
    ) where

import Control.DeepSeq
import Data.String
import qualified GHC.Generics as G
import qualified GHCJS.Marshal.Pure as J
import Glazier.React

-- | Returns a reference to the javascript *class* definition of the react component
newtype InputComponent = InputComponent JSVal
    deriving (G.Generic, Show, IsJSVal, J.PToJSVal, ToJS, IsString, NFData)

instance Component InputComponent where
  componentName _ = "input"

-- | This returns the javascript class definition of InputComponent.
-- There is ever only one InputComponent class, so it is purely available
inputComponent :: InputComponent
inputComponent = InputComponent js_inputComponent

#ifdef __GHCJS__

foreign import javascript unsafe
    "$r = hgr$InputComponent();"
    js_inputComponent :: JSVal

#else

js_inputComponent :: JSVal
js_inputComponent = nullRef

#endif
