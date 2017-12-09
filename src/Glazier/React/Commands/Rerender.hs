-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Glazier.React.Commands.Rerender where

-- import qualified GHCJS.Types as J
-- import qualified GHCJS.Marshal.Pure as J
import qualified Glazier.React as R
import qualified JavaScript.Extras as JE

-- newtype ComponentRef = ComponentRef J.JSVal
--     deriving (R.Dispose, J.PToJSVal, JE.ToJS)
-- instance J.IsJSVal ComponentRef

data Rerender = Rerender R.ReactComponent [JE.Property]
