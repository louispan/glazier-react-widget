{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Glazier.React.Framework.Effect.SetProperty.Exec where

import Glazier.React.Framework.Effect.SetProperty
import qualified JavaScript.Extras as JE

execSetProperty :: SetProperty -> IO ()
execSetProperty (SetProperty prop j) = JE.setProperty prop j
