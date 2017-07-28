module Glazier.React.Framework.Shared where

import Control.Lens
import Control.Concurrent.STM.TMVar
import qualified Glazier.React as R

-- | Something that has an immutable component, as well as a TMVar that
-- can be used to share a value with other threads.
-- This is used by the gadget to be able to purely manipulate a value
-- as well as put it into an TMVar for other threads to access the value.
newtype Shared i = Shared
    { runShared :: (i, TMVar i)
    }

_Shared :: Iso
    (Shared i)
    (Shared i')
    (i, TMVar i)
    (i', TMVar i')
_Shared = iso runShared Shared

instance R.Dispose i => R.Dispose (Shared i) where
    dispose (Shared (i, _)) = R.dispose i

-- | (tm)utable (var)iable
tmvar :: Lens' (Shared i) (TMVar i)
tmvar = _Shared . _2

-- | (i)mmutable (val)ue
ival :: Lens' (Shared i) i
ival = _Shared . _1
