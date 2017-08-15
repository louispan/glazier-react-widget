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

newtype Builder (a :: [Type]) atrs (d :: [Type]) dtls (p :: [Type]) plns =
    Builder ( Many atrs -> F R.Reactor (Many d) -- make details
            , Many dtls -> STM (Many a) -- from details
            , F R.Reactor (Many p)) -- make plans

andBuilder
    :: Builder a1 atrs d1 dtls p1 plns
    -> Builder a2 atrs d2 dtls p2 plns
    -> Builder (Append a1 a2) atrs (Append d1 d2) dtls (Append p1 p2) plns
andBuilder (Builder (mkDtl, fromDtl, mkPln)) (Builder (mkDtl', fromDtl', mkPln')) =
    Builder ( \o -> (/./) <$> mkDtl o <*> mkDtl' o
            , \d -> (/./) <$> fromDtl d <*> fromDtl' d
            , (/./) <$> mkPln <*> mkPln')

-- | Identity for 'andBuild'
idle :: Builder '[] atrs '[] dtls '[] plns
idle = Builder ( const $ pure nil
               , const (pure nil)
               , pure nil)

-- | Add a type @o@ into the factory
build
    :: (UniqueMember o atrs, UniqueMember o dtls)
    => Proxy o -> Builder '[o] atrs '[o] dtls '[] plns
build _ =
    Builder
        ( pure . single . fetch
        , pure . single . fetch
        , pure nil)
