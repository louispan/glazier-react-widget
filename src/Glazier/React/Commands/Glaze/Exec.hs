{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Glazier.React.Commands.Glaze.Exec where

import Control.Concurrent.STM
import Control.Monad.Free.Church
import Control.Monad.Trans
import Data.Diverse
import Data.Proxy
import qualified Glazier.React as R
import Glazier.React.Framework as F
import Glazier.React.Commands.Glaze

execGlaze
    :: ( UniqueMember (GlazeCommand a) cmds
       , UniqueMember (TMVar Int) envs
       , UniqueMember R.ReactComponent envs
       )
    => F.Executor IO '[GlazeCommand a] cmds '[TMVar Int, R.ReactComponent] envs
execGlaze = F.executor Proxy $ \env (GlazeCommand output mks) ->
    let muid = fetch env
        comp = fetch env
    in lift $ iterM (R.runGlaze muid comp output) mks
