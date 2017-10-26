{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Framework.Trigger where

import qualified Control.Parameterized as P
import Data.Diverse
import qualified Data.DList as DL
import qualified GHCJS.Types as J

newtype Trigger a =
    Trigger { runTrigger :: (J.JSString, J.JSVal -> IO (DL.DList a)) }
    deriving Functor

-- trigger1 :: J.JSString -> (J.JSVal -> IO a) -> Trigger a
-- trigger1 n f = Trigger (n, fmap DL.singleton <$> f)

-- | A list of triggers
newtype Triggers a = Triggers
    { runTriggers :: DL.DList (Trigger a)
    }

instance P.PEmpty Triggers (Which '[]) where
    pempty' = Triggers mempty

-- | UndecidableInstance!
-- It is okay for more than one trigger to results in the same action, hence the use of @AppendUnique@
instance (Diversify a c, Diversify b c, c ~ AppendUnique a b) =>
         P.PSemigroup Triggers (Which a) (Which b) (Which c) where
    pappend' (Triggers x) (Triggers y) =
        Triggers $ (fmap diversify <$> x) `DL.append` (fmap diversify <$> y)
