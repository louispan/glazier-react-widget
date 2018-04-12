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

import Data.Diverse.Lens
import Glazier.React.Effect.Concur.Internal

type ConcurCmds c =
    '[ ForkConcur c
    ]

type AsConcur c =
    ( AsFacet (ForkConcur c) c
    , AsFacet [c] c
    )
