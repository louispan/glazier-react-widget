{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Glazier.React.Widgets.Collection where

import Control.Lens
import Data.Foldable
import Glazier.React
import qualified Data.JSString as J
import qualified JavaScript.Extras as JE

collectionWindow :: (Functor t, Foldable t)
    => ReactId -> Window (t (Obj s)) ()
collectionWindow k = do
    ss <- view _model
    let displayItem s = Als $ (displayWeakObj $ weakObj s)
    bh "ul" [("key", JE.toJSRep . J.pack $ show k)] (getAls (fold $ displayItem <$> ss))
