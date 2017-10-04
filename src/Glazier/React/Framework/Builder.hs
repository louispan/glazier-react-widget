{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Framework.Builder where

import Data.Diverse
import Data.Kind
import Data.Proxy

newtype Builder m (r :: [Type]) reqs (s :: [Type]) specs =
    Builder ( Many specs -> m (Many r) -- from specifications
            , Many reqs -> m (Many s) -- make inactive specifications
            )

-- | identity for 'andBuild'
idle :: Applicative m => Builder m '[] reqs '[] specs
idle = Builder (const $ pure nil, const $ pure nil)

andBuilder
    :: Applicative m
    => Builder m r1 reqs s1 specs
    -> Builder m r2 reqs s2 specs
    -> Builder m (Append r1 r2) reqs (Append s1 s2) specs
andBuilder (Builder (fromSpec, mkSpec)) (Builder (fromSpec', mkSpec')) =
    Builder ( \s -> (/./) <$> fromSpec s <*> fromSpec' s
            , \r -> (/./) <$> mkSpec r <*> mkSpec' r
            )

-- | Add a type @x@ into the factory
build
    :: (Applicative m, UniqueMember x reqs, UniqueMember x specs)
    => Proxy x -> Builder m '[x] reqs '[x] specs
build _ = Builder ( pure . single . fetch
                  , pure . single . fetch
                  )
