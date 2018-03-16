{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Glazier.React.Framework.Core.MkId.Internal where

import Control.Monad.State
import qualified Data.JSString as J
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
import qualified JavaScript.Extras as JE

import Data.String
-- | A monad that can safely create Widgets with unique
-- 'GadgetId' and 'PlanId'
-- prototype :: MonadState Int p => p (Widget w x s m c)
-- protot

mkId :: MonadState Int m => J.JSString -> m J.JSString
mkId n = do
    i <- get
    let i' = JE.safeModularIncrement i
    put i'
    pure . J.append n . J.cons ':' . J.pack $ show i'

-- the base monad bind:
-- returns a STM async
-- which actually creates a STM Ref to fire once-off results into
-- for the pure thing to read?

-- This id can also be used as the react @key@
newtype GizmoId = GizmoId { unGizmoId :: J.JSString }
    deriving (Read, Show, Eq, Ord, JE.ToJS, JE.FromJS, IsString, J.IsJSVal, J.PToJSVal)

newtype PlanId = PlanId { unPlanId :: J.JSString }
    deriving (Read, Show, Eq, Ord, JE.ToJS, JE.FromJS, IsString, J.IsJSVal, J.PToJSVal)

mkGizmoId :: MonadState Int m => J.JSString -> m GizmoId
mkGizmoId n = GizmoId <$> mkId n

mkPlanId :: MonadState Int m => J.JSString -> m PlanId
mkPlanId n = PlanId <$> mkId n