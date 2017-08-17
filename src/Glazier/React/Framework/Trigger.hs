{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Framework.Trigger where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad.Trans.Maybe
import Data.Diverse.Lens
import qualified Data.DList as D
import Data.Kind
import qualified Data.Map.Strict as M
import Data.Proxy
import Data.Semigroup (Semigroup(..))
import GHC.Exts (Any, coerce)
import qualified GHCJS.Types as J
import qualified Glazier.React.Framework.Widget as F
import Unsafe.Coerce

newtype Triggers specs = Triggers (M.Map J.JSString (TMVar (F.Design specs) -> J.JSVal -> IO ()))

instance Semigroup (Triggers spec) where
    (Triggers trigs1) <> (Triggers trigs2) = Triggers (M.unionWith go trigs1 trigs2)
      where
        go f g d j = f d j >> g d j

instance Monoid (Triggers spec) where
    mappend = (<>)
    mempty = Triggers M.empty
