-- {-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Framework.Core.Trigger where

import Control.Arrow
import Control.DeepSeq
import Control.Lens
import Data.Diverse.Profunctor
import Data.Generics.Product
import Data.Semigroup
import qualified GHCJS.Types as J
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Core.Activator as F
import qualified Glazier.React.Framework.Core.Gate as F
import qualified Glazier.React.Framework.Core.Handler as F
import qualified Glazier.React.Framework.Core.Model as F
import qualified Glazier.React.Framework.Core.Obj as F
import qualified Glazier.React.Framework.Core.Topic as F

------------------------------------------------------

-- | Create callbacks and add it to this state's dlist of listeners.
-- @AllowAmbiguousTypes@: Use @TypeApplications@ instead of @Proxy@ to specify @t@
trigger :: forall t m v s c.
    ( R.MonadReactor m
    , NFData c
    , HasItemTag' t [R.Listener] s
    )
    => J.JSString
    -> (J.JSVal -> IO c)
    -> F.ProtoActivator m v s c
trigger n f = F.Topic $ \(F.Obj ref its) -> F.Gate $ \k _ -> do
    (ds, cb) <- R.doMkCallback f k
    R.doModifyIORef' ref $ \obj ->
        obj & its.F.model.itemTag' @t %~ ((n, cb) :)
            & its.F.plan.field @"disposeOnRemoved" %~ (<> ds)

-- | Convenience function to create an activator
-- given triggers and a handler.
controlledTrigger :: forall t m v s a b.
    ( R.MonadReactor m
    , NFData a
    , HasItemTag' t [R.Listener] s
    )
    => J.JSString
    -> (J.JSVal -> IO a)
    -> F.ProtoHandler m v s a b
    -> F.ProtoActivator m v s b
controlledTrigger n f hdl = (trigger @t n f) >>> hdl

-- -- | Convenience function to create an activator
-- -- given triggers and a handler.
-- -- Complex version using 'controls'
-- controlledTrigger :: forall t m v s c1 c2 c3 c4 a b.
--     ( R.MonadReactor m
--     , NFData (Which c2)
--     , HasItemTag' t [R.Listener] s
--     , c4 ~ AppendUnique c1 c3
--     , Injected a c2 b c3
--     , Diversify c1 c4
--     , Diversify c3 c4
--     )
--     => J.JSString
--     -> (J.JSVal -> IO (DL.DList (Which c2)))
--     -> ProtoHandler m v s (Which c1) (Which a) (Which b)
--     -> ProtoActivator m v s (Which c4)
-- controlledTrigger n f hdl = hdl `controls` trigger @t n f
