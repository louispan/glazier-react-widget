{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Glazier.React.Commands.Property.Exec where

import Control.Monad.Trans
import Data.Diverse
import qualified JavaScript.Extras as JE
import Glazier.React.Framework as F
import Glazier.React.Commands.Property

execProperty :: UniqueMember SetPropertyCommand cmds => F.Executor IO '[SetPropertyCommand] cmds '[] envs
execProperty = F.executor' $ \(SetPropertyCommand j prop) -> lift $ JE.setProperty prop j
