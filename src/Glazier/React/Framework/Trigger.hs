{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Framework.Trigger where

import Data.Diverse
import qualified Data.DList as DL
import qualified GHCJS.Types as J
import qualified Parameterized.Data.Monoid as P

newtype Trigger a =
    Trigger { runTrigger :: (J.JSString, J.JSVal -> IO (DL.DList a)) }
    deriving Functor

-- | A list of triggers
newtype Triggers a = Triggers
    { runTriggers :: DL.DList (Trigger a)
    }

type instance P.PNullary Triggers r = Triggers r

instance P.PMEmpty Triggers (Which '[]) where
    pmempty = Triggers mempty

-- | UndecidableInstance!
-- It is okay for more than one trigger to results in the same action, hence the use of @AppendUnique@
instance (Diversify a c, Diversify b c, c ~ AppendUnique a b) =>
         P.PSemigroup Triggers (Which a) (Which b) (Which c) where
    pmappend (Triggers x) (Triggers y) =
        Triggers $ (fmap diversify <$> x) `DL.append` (fmap diversify <$> y)
