{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Framework.Trigger where

import Data.Diverse
import qualified Data.DList as DL
import qualified GHCJS.Types as J

newtype Trigger a =
    Trigger { runTrigger :: (J.JSString, J.JSVal -> IO (DL.DList a)) }
    deriving Functor

type Triggers a = DL.DList (Trigger a)

-- | identity for 'andBuild'
boring :: Triggers (Which '[])
boring = mempty

-- | It is okay for more than one trigger to results in the same action, hence the use of @AppendUnique@
andTriggers
    :: (Diversify a1 (AppendUnique a1 a2), Diversify a2 (AppendUnique a1 a2))
    => Triggers (Which a1)
    -> Triggers (Which a2)
    -> Triggers (Which (AppendUnique a1 a2))
andTriggers f g =
    (fmap diversify <$> f) `DL.append`
    (fmap diversify <$> g)

trigger :: J.JSString -> (J.JSVal -> IO a) -> Trigger a
trigger n f = Trigger (n, fmap DL.singleton <$> f)
