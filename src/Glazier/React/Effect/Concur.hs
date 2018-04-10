{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Effect.Concur
    ( ConcurCmds
    , AsConcur
    , ForkConcur(..)
    , Concur -- constructor not exported
    , concur
    , evalConcur
    )
    where

import Control.Concurrent.STM
import Control.Monad.State.Strict
import Data.Diverse.Lens
import qualified Data.DList as DL
import Glazier.React.Effect.Concur.Internal

type ConcurCmds c =
    '[ ForkConcur c
    ]

type AsConcur c =
    ( AsFacet (ForkConcur c) c
    , AsFacet [c] c
    )
