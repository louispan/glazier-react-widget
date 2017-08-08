{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Framework.Build where

import Control.Concurrent.STM
import Control.Monad.Free.Church
import Data.Diverse.Lens
import Data.Kind
import Data.Proxy
import qualified Glazier.React as R

newtype Build (o :: [Type]) ols (d :: [Type]) dtls (p :: [Type]) plns acts =
    Build ( Many ols -> F (R.Glaze (Which acts)) (Many d) -- make details
          , Many dtls -> STM (Many o) -- from details
          , F (R.Glaze (Which acts)) (Many p)) -- make plans

andBuild
    :: Build o1 ols d1 dtls p1 plns acts
    -> Build o2 ols d2 dtls p2 plns acts
    -> Build (Append o1 o2) ols (Append d1 d2) dtls (Append p1 p2) plns acts
andBuild (Build (mkDtl, fromDtl, mkPln)) (Build (mkDtl', fromDtl', mkPln')) =
        Build ( \o -> (/./) <$> mkDtl o <*> mkDtl' o
                , \d -> (/./) <$> fromDtl d <*> fromDtl' d
                , (/./) <$> mkPln <*> mkPln')

-- | Identity for 'andBuild'
idle :: Build '[] ols '[] dtls '[] plns acts
idle = Build ( const $ pure nil
               , const (pure nil)
               , pure nil)

-- | Add a type @o@ into the factory
build
    :: forall o ols dtls plns acts.
       (UniqueMember o ols, UniqueMember o dtls)
    => Proxy o -> Build '[o] ols '[o] dtls '[] plns acts
build _ =
    Build
        ( pure . single . fetch @o
        , pure . single . fetch @o
        , pure nil)
