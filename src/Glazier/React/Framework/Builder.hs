{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Framework.Builder where

import Control.Concurrent.STM
import Control.Monad.Free.Church
import Data.Diverse.Lens
import Data.Kind
import Data.Proxy
import qualified Glazier.React as R

newtype Builder (o :: [Type]) ols (d :: [Type]) dtls (p :: [Type]) plns acts =
    Builder ( Many ols -> F (R.Glaze (Which acts)) (Many d) -- make details
            , Many dtls -> STM (Many o) -- from details
            , F (R.Glaze (Which acts)) (Many p)) -- make plans

andBuilder
    :: Builder o1 ols d1 dtls p1 plns acts
    -> Builder o2 ols d2 dtls p2 plns acts
    -> Builder (Append o1 o2) ols (Append d1 d2) dtls (Append p1 p2) plns acts
andBuilder (Builder (mkDtl, fromDtl, mkPln)) (Builder (mkDtl', fromDtl', mkPln')) =
    Builder ( \o -> (/./) <$> mkDtl o <*> mkDtl' o
            , \d -> (/./) <$> fromDtl d <*> fromDtl' d
            , (/./) <$> mkPln <*> mkPln')

-- | Identity for 'andBuild'
idle :: Builder '[] ols '[] dtls '[] plns acts
idle = Builder ( const $ pure nil
               , const (pure nil)
               , pure nil)

-- | Add a type @o@ into the factory
builder
    :: (UniqueMember o ols, UniqueMember o dtls)
    => Proxy o -> Builder '[o] ols '[o] dtls '[] plns acts
builder _ =
    Builder
        ( pure . single . fetch
        , pure . single . fetch
        , pure nil)
