{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Glazier.React.Widgets.Collection where

import Control.Lens
import Data.Foldable
import Glazier.React

collectionWindow :: (Functor t, Foldable t)
    => ReactId -> Window (t (Obj s)) ()
collectionWindow k = do
    ss <- view _model
    let displayItem s = Als $ (displayObj s)
    bh "ul" [("key", reactIdKey' k)] (getAls (fold $ displayItem <$> ss))
