{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Framework.Factory where

import Control.Monad.Free.Church
import Data.Diverse.Lens
import Data.Kind
import Data.Proxy
import qualified Glazier.React as R

type MkPlan plns acts = F (R.Maker (Which acts)) (Many plns)
type MkDetail dtls ols acts = Many ols -> F (R.Maker (Which acts)) (Many dtls)
type FromDetail ols dtls = Many dtls -> Many ols

newtype Factory (o :: [Type]) ols (d :: [Type]) dtls (p :: [Type]) plns acts =
    Factory (MkDetail d ols acts, FromDetail o dtls, MkPlan p acts)

andFactory
    :: Factory o1 ols d1 dtls p1 plns acts
    -> Factory o2 ols d2 dtls p2 plns acts
    -> Factory (Append o1 o2) ols (Append d1 d2) dtls (Append p1 p2) plns acts
andFactory (Factory (mkDtl, fromDtl, mkPln)) (Factory (mkDtl', fromDtl', mkPln')) =
        Factory ( \o -> (/./) <$> mkDtl o <*> mkDtl' o
                , \d -> fromDtl d /./ fromDtl' d
                , (/./) <$> mkPln <*> mkPln')

-- | Identity for 'andFactory'
idle :: Factory '[] ols '[] dtls '[] plns acts
idle = Factory ( const $ pure nil
               , const nil
               , pure nil)

-- | Add a type @o@ into the factory
factory
    :: forall o ols dtls plns acts.
       (UniqueMember o ols, UniqueMember o dtls)
    => Proxy o -> Factory '[o] ols '[o] dtls '[] plns acts
factory _ =
    Factory
        ( pure . single . fetch @o
        , single . fetch @o
        , pure nil)
