module Glazier.React.Commands.Focus where

import Control.Lens
import Control.DeepSeq
import Data.Diverse.Lens
import Data.IORef
import qualified GHC.Generics as G
import qualified Data.DList as DL
import qualified Glazier.React as R
import qualified Glazier.React.Framework as F
import qualified GHCJS.Types as J

newtype Focus = Focus R.EventTarget

-- whenTodoEditRef :: (R.MonadReactor x m)
--   => IORef v
--   -> Lens' v (F.ComponentPlan x m, TodoModel)
--   -> TodoEditRef
--   -> m (DL.DList c)
-- whenTodoEditRef ref this (TodoEditRef j) = do
--        R.doModifyIORef' ref (set' (this._2._1.field @"editRef") j)
--        pure mempty
