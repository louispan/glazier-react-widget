{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Glazier.React.Commands.SetProperty.Exec where

import qualified JavaScript.Extras as JE
import Glazier.React.Commands.SetProperty

execSetProperty :: SetProperty -> IO ()
execSetProperty (SetProperty j prop) = JE.setProperty prop j
