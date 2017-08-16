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

newtype Builder (r :: [Type]) reqs (s :: [Type]) specs =
    Builder ( Many reqs -> F R.Reactor (Many s) -- make specifications
            , Many specs -> STM (Many r) -- from details
            )

andBuilder
    :: Builder r1 reqs s1 specs
    -> Builder r2 reqs s2 specs
    -> Builder (Append r1 r2) reqs (Append s1 s2) specs
andBuilder (Builder (mkSpec, fromSpec)) (Builder (mkSpec', fromSpec')) =
    Builder ( \o -> (/./) <$> mkSpec o <*> mkSpec' o
            , \d -> (/./) <$> fromSpec d <*> fromSpec' d
            )

-- | Identity for 'andBuild'
idle :: Builder '[] reqs '[] specs
idle = Builder ( const $ pure nil
               , const (pure nil)
               )

-- | Add a type @x@ into the factory
build :: (UniqueMember x reqs, UniqueMember x specs) => Proxy x -> Builder '[x] reqs '[x] specs
build _ = Builder ( pure . single . fetch
                  , pure . single . fetch
                  )
