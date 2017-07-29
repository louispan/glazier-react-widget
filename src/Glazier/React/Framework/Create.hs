{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Framework.Create where

import Control.Monad.Free.Church
import Data.Diverse.Lens
import Data.Kind
import Data.Proxy
import qualified Glazier.React as R

type MkPlan plns acts = F (R.Maker (Which acts)) (Many plns)
type MkDetail dtls ols acts = Many ols -> F (R.Maker (Which acts)) (Many dtls)
type FromDetail ols dtls = Many dtls -> Many ols

newtype Create (o :: [Type]) ols (d :: [Type]) dtls (p :: [Type]) plns acts =
    Create (MkDetail d ols acts, FromDetail o dtls, MkPlan p acts)

andCreate
    :: Create o1 ols d1 dtls p1 plns acts
    -> Create o2 ols d2 dtls p2 plns acts
    -> Create (Append o1 o2) ols (Append d1 d2) dtls (Append p1 p2) plns acts
andCreate (Create (mkDtl, fromDtl, mkPln)) (Create (mkDtl', fromDtl', mkPln')) =
        Create ( \o -> (/./) <$> mkDtl o <*> mkDtl' o
                , \d -> fromDtl d /./ fromDtl' d
                , (/./) <$> mkPln <*> mkPln')

-- | Identity for 'andCreate'
idle :: Create '[] ols '[] dtls '[] plns acts
idle = Create ( const $ pure nil
               , const nil
               , pure nil)

-- | Add a type @o@ into the factory
create
    :: forall o ols dtls plns acts.
       (UniqueMember o ols, UniqueMember o dtls)
    => Proxy o -> Create '[o] ols '[o] dtls '[] plns acts
create _ =
    Create
        ( pure . single . fetch @o
        , single . fetch @o
        , pure nil)
