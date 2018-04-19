{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Effect.Concur
    ( ConcurCmd(..)
    , Concur -- constructor not exported
    , concur
    , evalConcur
    )
    where

-- import Data.Diverse.Lens
import Glazier.React.Effect.Concur.Internal

-- type ConcurCmds c = '[[c], ForkConcur c]

-- type AsConcur c =
--     ( AsFacet [c] c
--     , AsFacet (ForkConcur c) c
--     )
