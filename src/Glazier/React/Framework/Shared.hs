module Glazier.React.Framework.Shared where

import Control.Lens
import Control.Concurrent.STM.TMVar
import qualified Glazier.React as R

-- | Something that has an immutable component, as well as a TMVar that
-- can be used to share a value with other threads.
-- This is used by the gadget to be able to purely manipulate a value
-- as well as put it into an TMVar for other threads to access the value.
newtype Shared i v = Shared
    { runShared :: (i, ReifiedLens' v i, TMVar v)
    }

_Shared :: Iso
    (Shared i v)
    (Shared i' v')
    (i, ReifiedLens' v i, TMVar v)
    (i', ReifiedLens' v' i', TMVar v')
_Shared = iso runShared Shared

instance R.Dispose i => R.Dispose (Shared i v) where
    dispose (Shared (i, _, _)) = R.dispose i

-- | (tm)utable (var)iable
tmvar :: Lens' (Shared i v) (TMVar v)
tmvar = _Shared . _3

-- | (i)mmutable (val)ue
ival :: Lens' (Shared i v) i
ival = _Shared . _1

-- | tm(v)ar to immutable (v)alue (lens)
vlens :: Lens' (Shared i v) (ReifiedLens' v i)
vlens = _Shared . _2
