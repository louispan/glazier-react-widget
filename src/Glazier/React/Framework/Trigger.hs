{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Framework.Trigger where

import Data.Diverse
import qualified Data.DList as DL
import qualified GHCJS.Types as J
import qualified Glazier.React.Framework.Core as F

newtype Trigger a =
    Trigger { runTrigger :: (J.JSString, J.JSVal -> IO (DL.DList a)) }
    deriving Functor

trigger1 :: J.JSString -> (J.JSVal -> IO a) -> Trigger a
trigger1 n f = Trigger (n, fmap DL.singleton <$> f)

-- | A list of trigger, and a value so we can have a monad.
newtype Triggers a r = Triggers
    { runTriggers :: (DL.DList (Trigger a), r)
    } deriving (Functor, Applicative, Monad)

instance F.PPointed Triggers (Which '[]) where
    ppure a = Triggers (mempty, a)

-- | It is okay for more than one trigger to results in the same action, hence the use of @AppendUnique@
-- UndecidableInstance!
instance ( Diversify a c
         , Diversify b c
         , c ~ AppendUnique a b
         ) =>
         F.PApplicative Triggers (Which a) (Which b) (Which c) where
    papply (Triggers (x, f)) (Triggers (y, a)) =
        Triggers
            ((fmap diversify <$> x) `DL.append` (fmap diversify <$> y), f a)

-- | UndecidableInstance!
instance (Diversify a c, Diversify b c, c ~ AppendUnique a b) =>
         F.PMonad Triggers (Which a) (Which b) (Which c) where
    pbind (Triggers (x, a)) k =
        let Triggers (y, b) = k a
        in Triggers
               ((fmap diversify <$> x) `DL.append` (fmap diversify <$> y), b)
