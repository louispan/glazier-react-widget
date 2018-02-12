{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Glazier.React.Framework.Effect.GetProperty.Exec where

import Glazier.React.Framework.Effect.GetProperty
import qualified JavaScript.Extras as JE

execGetProperty :: (m () -> IO ()) -> GetProperty m -> IO ()
execGetProperty run (GetProperty n j k) = JE.getProperty n j >>= (run . k)
