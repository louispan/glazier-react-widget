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
import qualified Glazier.React.Framework.Public as F
import qualified Glazier.React.Framework.Trigger as F
import qualified Glazier.React.Framework.Widget as F

-- (t + a) is the set of actions that need to be handled.
-- h is the set of action handlers.
-- h can be smaller than (t + a) if we are given external handlers for (t + a - h)
-- t can be smaller than h but (t - h) must be included inside p, otherwise a compile error (for best practice)
-- The external event interface of Prototype is that it exposes a Handler for p
-- p must contain only things inside h. It cannot contain things that are handled by external handlers.

newtype Prototype m (r :: [Type]) reqs
                    (s :: [Type]) specs
                    (a :: [Type]) as
                    (t :: [Type]) ts
                    (p :: [Type]) (h :: [Type]) hs
                    (ac :: [Type]) (hc :: [Type]) cmds =
    Prototype ( F.Display m specs
              , F.Builder m r reqs s specs
              -- activator contains other prerequisites
              -- of executor, and actions that need to be handled
              , F.Activator m (F.Design specs) a as ac cmds
              , F.Triggers m t ts
              , F.Handler m (F.Design specs) h hs hc cmds
              , F.Public p
              )

-- | identity for 'andPrototype'
dummy :: Monad m => Prototype m '[] reqs '[] specs '[] as '[] ts '[] '[] hs '[] '[] cmds
dummy = Prototype (mempty, F.idle, F.inert, F.boring, F.ignore, F.private)

-- | The action and command types are merged, not appended
andPrototype
    :: Monad m
    => Prototype m r1 reqs s1 specs a1 as t1 ts p1 h1 hs ac1 hc1 cmds
    -> Prototype m r2 reqs s2 specs a2 as t2 ts p2 h2 hs ac2 hc2 cmds
    -> Prototype m (Append r1 r2) reqs
                   (Append s1 s2) specs
                   (AppendUnique a1 a2) as
                   (AppendUnique t1 t2) ts
                   (AppendUnique p1 p2) (Append h1 h2) hs
                   (AppendUnique ac1 ac2) (AppendUnique hc1 hc2) cmds
andPrototype (Prototype (d, b, a, t, h, p)) (Prototype (d', b', a', t', h', p')) =
    Prototype
        ( d <> d'
        , b `F.andBuilder` b'
        , a `F.andActivator` a'
        , t `F.andTriggers` t'
        , h `F.orHandler` h'
        , p `F.andPublic` p'
        )

displaying
    :: Monad m
    => F.Display m specs
    -> Prototype m '[] reqs '[] specs '[] as '[] ts '[] '[] hs '[] '[] cmds
displaying d = Prototype (d, F.idle, F.inert, F.boring, F.ignore, F.private)

building
    :: Monad m
    => F.Builder m r reqs s specs
    -> Prototype m r reqs s specs '[] as '[] ts '[] '[] hs '[] '[] cmds
building b = Prototype (mempty, b, F.inert, F.boring, F.ignore, F.private)

activating
    :: Monad m
    => F.Activator m (F.Design specs) a as ac cmds
    -> Prototype m '[] reqs '[] specs a as '[] ts '[] '[] hs ac '[] cmds
activating a = Prototype (mempty, F.idle, a, F.boring, F.ignore, F.private)

triggering
    :: Monad m
    => F.Triggers m t ts
    -> Prototype m '[] reqs '[] specs '[] as t ts '[] '[] hs '[] '[] cmds
triggering t = Prototype (mempty, F.idle, F.inert, t, F.ignore, F.private)

handling
    :: Monad m
    => F.Handler m (F.Design specs) h hs hc cmds
    -> Prototype m '[] reqs '[] specs '[] as '[] ts '[] h hs '[] hc cmds
handling h = Prototype (mempty, F.idle, F.inert, F.boring, h, F.private)

publically :: Monad m => F.Public p -> Prototype m '[] reqs '[] specs '[] as '[] ts p '[] hs '[] '[] cmds
publically p = Prototype (mempty, F.idle, F.inert, F.boring, F.ignore, p)
