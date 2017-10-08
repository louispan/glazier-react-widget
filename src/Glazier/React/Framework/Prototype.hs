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
import qualified Glazier.React.Framework.Handler as F
import qualified Glazier.React.Framework.Trigger as F
import qualified Glazier.React.Framework.Widget as F

-- (t + a) is the set of actions that need to be handled.
-- h is the set of action handlers.
-- h can be smaller than (t + a) if we are given external handlers for (t + a - h)
-- t can be smaller than h but (t - h) must be included inside p, otherwise a compile error (for best practice)
-- The external event interface of Prototype is that it exposes a Handler for p
-- p must contain only things inside h. It cannot contain things that are handled by external handlers.

newtype Prototype m (r :: [Type]) (reqs :: [Type])
                    (s :: [Type]) (specs :: [Type])
                    (h :: [Type]) (hs :: [Type])
                    (t :: [Type]) (ts :: [Type])
                    (a :: [Type]) (as :: [Type])
                    (hc :: [Type]) (hcs :: [Type]) (acs :: [Type]) =
    Prototype ( F.Display m specs
              , F.Builder m r reqs s specs
              , F.Handler m (F.Design specs) h hs hc hcs
              -- activator contains other prerequisites
              -- of executor, and actions that need to be handled
              , F.Triggers t ts
              , F.Activator m (F.Design specs) a as acs
              )

-- | identity for 'andPrototype'
dummy :: Monad m => Prototype m '[] reqs '[] specs '[] hs '[] ts '[] as '[] hcs '[]
dummy = Prototype (mempty, F.idle, F.ignore, F.boring, F.inert)

-- | The action and command types are merged, not appended
andPrototype
    :: Monad m
    => Prototype m r1 reqs s1 specs h1 hs t1 ts a1 as hc1 hcs acs
    -> Prototype m r2 reqs s2 specs h2 hs t2 ts a2 as hc2 hcs acs
    -> Prototype m (Append r1 r2) reqs
                   (Append s1 s2) specs
                   (Append h1 h2) hs
                   (AppendUnique t1 t2) ts
                   (AppendUnique a1 a2) as
                   (AppendUnique hc1 hc2) hcs acs
andPrototype (Prototype (d, b, h, t, a)) (Prototype (d', b', h', t', a')) =
    Prototype
        ( d <> d'
        , b `F.andBuilder` b'
        , h `F.orHandler` h'
        , t `F.andTriggers` t'
        , a `F.andActivator` a'
        )

displaying
    :: Monad m
    => F.Display m specs
    -> Prototype m '[] reqs '[] specs '[] hs '[] ts '[] as '[] hcs '[]
displaying d = Prototype (d, F.idle, F.ignore, F.boring, F.inert)

building
    :: Monad m
    => F.Builder m r reqs s specs
    -> Prototype m r reqs s specs '[] hs '[] ts '[] as '[] hcs '[]
building b = Prototype (mempty, b, F.ignore, F.boring, F.inert)

activating
    :: Monad m
    => F.Activator m (F.Design specs) a as acs
    -> Prototype m '[] reqs '[] specs '[] hs '[] ts a as '[] hcs acs
activating a = Prototype (mempty, F.idle, F.ignore, F.boring, a)

triggering
    :: Monad m
    => F.Triggers t ts
    -> Prototype m '[] reqs '[] specs '[] hs t ts '[] as '[] hcs '[]
triggering t = Prototype (mempty, F.idle, F.ignore, t, F.inert)

handling
    :: Monad m
    => F.Handler m (F.Design specs) h hs hc hcs
    -> Prototype m '[] reqs '[] specs h hs '[] ts '[] as hc hcs '[]
handling h = Prototype (mempty, F.idle, h, F.boring, F.inert)
