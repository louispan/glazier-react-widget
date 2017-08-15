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

newtype Builder (a :: [Type]) atrs (d :: [Type]) dtls =
    Builder ( Many atrs -> F R.Reactor (Many d) -- make details
            , Many dtls -> STM (Many a) -- from details
            )

andBuilder
    :: Builder a1 atrs d1 dtls
    -> Builder a2 atrs d2 dtls
    -> Builder (Append a1 a2) atrs (Append d1 d2) dtls
andBuilder (Builder (mkDtl, fromDtl)) (Builder (mkDtl', fromDtl')) =
    Builder ( \o -> (/./) <$> mkDtl o <*> mkDtl' o
            , \d -> (/./) <$> fromDtl d <*> fromDtl' d
            )

-- | Identity for 'andBuild'
idle :: Builder '[] atrs '[] dtls
idle = Builder ( const $ pure nil
               , const (pure nil)
               )

-- | Add a type @o@ into the factory
build :: (UniqueMember o atrs, UniqueMember o dtls) => Proxy o -> Builder '[o] atrs '[o] dtls
build _ = Builder ( pure . single . fetch
                  , pure . single . fetch
                  )
