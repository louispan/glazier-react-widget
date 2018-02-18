{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Glazier.React.Framework.Effect.GetProperty.Exec where

import Glazier.React.Framework.Effect.GetProperty
import qualified JavaScript.Extras as JE

execGetProperty :: (forall a. m a -> IO a) -> GetProperty m -> IO ()
execGetProperty run (GetProperty n j k) = JE.getProperty n j >>= (run . k)
