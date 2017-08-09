{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Glazier.React.Commands.Reactor.Exec where

import Control.Concurrent.STM
import Control.Monad.Free.Church
import Control.Monad.Trans
import Data.Diverse
import Data.Proxy
import qualified Glazier.React as R
import Glazier.React.Framework as F
import Glazier.React.Commands.Reactor

execReactor
    :: ( UniqueMember (ReactorCommand a) cmds
       , UniqueMember (TMVar Int) envs
       , UniqueMember R.ReactComponent envs
       )
    => F.Executor IO '[ReactorCommand a] cmds '[TMVar Int, R.ReactComponent] envs
execReactor = F.executor Proxy $ \env (ReactorCommand output mks) ->
    let muid = fetch env
        comp = fetch env
    in lift $ iterM (R.runReactor muid comp output) mks
