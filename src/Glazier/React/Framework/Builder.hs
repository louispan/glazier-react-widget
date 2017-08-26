{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Framework.Builder where

import Control.Lens
import Control.Concurrent.STM
import Control.Monad.Free.Church
import Data.Diverse.Lens
import Data.Kind
import Data.Proxy
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Widget as F

newtype Builder v (r :: [Type]) reqs (s :: [Type]) specs =
    Builder ( TMVar v -> ReifiedLens' v (F.Design specs) -> Many reqs -> F R.Reactor (Many s) -- make specifications
            , Many specs -> STM (Many r) -- from details
            )

andBuilder
    :: Builder v r1 reqs s1 specs
    -> Builder v r2 reqs s2 specs
    -> Builder v (Append r1 r2) reqs (Append s1 s2) specs
andBuilder (Builder (mkSpec, fromSpec)) (Builder (mkSpec', fromSpec')) =
    Builder ( \v l rs -> (/./) <$> mkSpec v l rs <*> mkSpec' v l rs
            , \d -> (/./) <$> fromSpec d <*> fromSpec' d
            )

-- | Identity for 'andBuild'
idle :: Builder v '[] reqs '[] specs
idle = Builder ( \_ _ -> const $ pure nil
               , const $ pure nil
               )

-- | Add a type @x@ into the factory
build :: (UniqueMember x reqs, UniqueMember x specs) => Proxy x -> Builder v '[x] reqs '[x] specs
build _ = Builder ( \_ _ -> pure . single . fetch
                  , pure . single . fetch
                  )
