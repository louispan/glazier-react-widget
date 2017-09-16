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
import Data.Diverse
import Data.Kind
import Data.Proxy

newtype Builder (r :: [Type]) reqs (s :: [Type]) specs =
    Builder ( Many specs -> STM (Many r) -- from specifications
            , Many reqs -> STM (Many s) -- make specifications
            )

-- | identity for 'andBuild'
idle :: Builder '[] reqs '[] specs
idle = Builder (const $ pure nil, const $ pure nil)

andBuilder
    :: Builder r1 reqs s1 specs
    -> Builder r2 reqs s2 specs
    -> Builder (Append r1 r2) reqs (Append s1 s2) specs
andBuilder (Builder (fromSpec, mkSpec)) (Builder (fromSpec', mkSpec')) =
    Builder ( \s -> (/./) <$> fromSpec s <*> fromSpec' s
            , \r -> (/./) <$> mkSpec r <*> mkSpec' r
            )

-- | Add a type @x@ into the factory
build
    :: (UniqueMember x reqs, UniqueMember x specs)
    => Proxy x -> Builder '[x] reqs '[x] specs
build _ = Builder ( pure . single . fetch
                  , pure . single . fetch
                  )
