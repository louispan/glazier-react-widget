{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Glazier.React.Commands.Make.Exec where

import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Free.Church
import Control.Monad.Trans
import Data.Diverse
import Data.Proxy
import qualified Glazier.React as R
import Glazier.React.Framework.Execute as F
import Glazier.React.Commands.Make
import qualified Pipes.Concurrent as PC

execMake
    :: ( UniqueMember (MakeCommand a) cmds
       , UniqueMember (TMVar Int) envs
       , UniqueMember R.ReactComponent envs
       )
    => F.Execute IO '[MakeCommand a] cmds '[TMVar Int, R.ReactComponent] envs
execMake = F.execute Proxy $ \env (MakeCommand output mks) ->
    let muid = fetch env
        comp = fetch env
    in lift $ iterM (R.execMaker muid comp output) mks
