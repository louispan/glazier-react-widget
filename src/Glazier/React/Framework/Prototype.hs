{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Framework.Prototype where

import Data.Diverse
import Data.Kind
-- import Data.Proxy
import Data.Semigroup (Semigroup(..))
import qualified Glazier.React.Framework.Activator as F
import qualified Glazier.React.Framework.Builder as F
import qualified Glazier.React.Framework.Display as F
-- import qualified Glazier.React.Framework.Executor as F
import qualified Glazier.React.Framework.Handler as F
import qualified Glazier.React.Framework.Trigger as F
import qualified Glazier.React.Framework.Widget as F

-- type Handler' s a c = TVar s -> Which a -> MayT STM (DL.DList (Which c))
-- type Activator' s acts cmds = F.Executor' cmds -- effectful interpreters
--             -> F.Handler' s acts cmds -- externally provided handlers
--             -> TVar s -- TVar that contains the specs
-- No Which a!!
--             -> MaybeT (F R.Reactor) (STM ())

-- FIXME must h be smaller than t?
-- ins = (t - h) + i -- ie handled trigger + public input
-- if a in h, pass to internal handler;
-- outs = (h - t) + o -- unhandled triggers + pubic output
-- if a in h, pass to internal handler; then if a in o, pass to provided output handler
newtype Prototype (r :: [Type]) reqs
                  (s :: [Type]) specs
                  (i :: [Type]) ins (o :: [Type]) outs
                  (t :: [Type]) (h :: [Type]) acts
                  (ac :: [Type]) (hc :: [Type]) cmds =
    Prototype ( F.Display specs
              , F.Builder r reqs s specs
              , F.Activator (F.Design specs) i ins o outs ac cmds
              , F.Triggers t acts
              , F.Handler (F.Design specs) h acts hc cmds)

-- instance Semigroup (Prototype '[] reqs '[] specs '[] '[] acts '[] cmds) where
--     _ <> _ = Prototype (mempty, mempty, mempty, mempty)

-- instance Monoid (Prototype '[] reqs '[] specs '[] '[] acts '[] cmds) where
--     mempty = Prototype (mempty, mempty, mempty, mempty)
--     mappend = (<>)

-- | identity for 'andPrototype'
dummy :: Prototype '[] reqs '[] specs '[] ins '[] outs '[] '[] acts '[] '[] cmds
dummy = Prototype (mempty, F.idle, F.inert, F.boring, F.ignore)

-- | The action and command types are merged, not appended
andPrototype
    :: Prototype r1 reqs s1 specs i1 ins o1 outs t1 h1 acts ac1 hc1 cmds
    -> Prototype r2 reqs s2 specs i2 ins o2 outs t2 h2 acts ac2 hc2 cmds
    -> Prototype (Append r1 r2) reqs
                 (Append s1 s2) specs
                 (AppendUnique i1 i2) ins
                 (AppendUnique o1 o2) outs
                 (AppendUnique t1 t2) (Append h1 h2) acts
                 (AppendUnique ac1 ac2) (AppendUnique hc1 hc2) cmds
andPrototype (Prototype (d, b, a, t, h)) (Prototype (d', b', a', t', h')) =
    Prototype
        ( d <> d'
        , b `F.andBuilder` b'
        , a `F.andActivator` a'
        , t `F.andTriggers` t'
        , h `F.orHandler` h'
        )

-- prototyping
--     :: F.Display specs
--     -> F.Builder r reqs s specs
--     -> F.Activator (F.Design specs) i ins o outs ac cmds
--     -> F.Triggers t acts
--     -> F.Handler (F.Design specs) h acts hc cmds
--     -> Prototype r reqs s specs i ins o outs t h acts (AppendUnique ac hc) cmds
-- prototyping disp
--             (F.Builder (frmSpec, mkSpecD))
--             activator
--             (F.Triggers (_, ts))
--             (F.Handler (ha, _, hdl)) =
--     Prototype
--         ( disp
--         , F.Builder (frmSpec, mkSpecD)
--         , activator'
--         , F.Triggers (Proxy, ts)
--         , F.Handler (ha, Proxy, hdl))
--   where
--     activator' exec gated = activator (F.Executor (Proxy, F.getExecutor exec)) gated

displaying :: F.Display specs -> Prototype '[] reqs '[] specs '[] ins '[] outs '[] '[] acts '[] '[] cmds
displaying d = Prototype (d, F.idle, F.inert, F.boring, F.ignore)

building :: F.Builder r reqs s specs -> Prototype r reqs s specs '[] ins '[] outs '[] '[] acts '[] '[] cmds
building b = Prototype (mempty, b, F.inert, F.boring, F.ignore)

activating :: F.Activator (F.Design specs) i ins o outs c cmds -> Prototype '[] reqs '[] specs i ins o outs '[] '[] acts c '[] cmds
activating a = Prototype (mempty, F.idle, a, F.boring, F.ignore)

triggering :: F.Triggers a acts -> Prototype '[] reqs '[] specs '[] ins '[] outs a '[] acts '[] '[] cmds
triggering t = Prototype (mempty, F.idle, F.inert, t, F.ignore)

handling :: F.Handler (F.Design specs) h acts c cmds -> Prototype '[] reqs '[] specs '[] ins '[] outs '[] h acts '[] c cmds
handling h = Prototype (mempty, F.idle, F.inert, F.boring, h)
