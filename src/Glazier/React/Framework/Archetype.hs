{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Framework.Archetype where

import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import Data.IORef
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Activator as F
import qualified Glazier.React.Framework.Builder as F
import qualified Glazier.React.Framework.Core as F
import qualified Glazier.React.Framework.Display as F
import qualified Glazier.React.Framework.Handler as F
import qualified Glazier.React.Framework.Prototype as F
import qualified GHCJS.Foreign.Callback.Internal as J
import qualified GHCJS.Types as J
import qualified JavaScript.Extras as JE

newtype Archetype m p s a b c = Archetype {
    runArchetype ::
           ( F.Builder m p s p s
           , F.Handler m s a b
           -- activator contains other prerequisites
           -- of executor, and actions that need to be handled
           , F.Activator m s c
           , F.Display m s ()
           )
    }

archetype :: R.MonadReactor m
    => F.Prototype m (F.ComponentModel, s) p s p s a b c
    -> Archetype m p (IORef (F.ComponentModel, s)) a b c
archetype (F.Prototype ( F.Builder (F.MkPlan mkPlan, F.MkModel mkModel)
                     , F.Handler hdl
                     , F.Activator act
                     , F.Display disp)) = Archetype
     ( F.Builder ( F.MkPlan (R.doReadIORef >=> (mkPlan . snd))
                 , F.MkModel $ \p -> do
                         s <- mkModel p
                         -- create a model with a dummy render for now
                         cm <- F.ComponentModel
                                 <$> R.getComponent
                                 <*> R.mkKey
                                 <*> pure (R.Renderer (J.Callback J.nullRef))
                                 <*> pure []
                         ref <- R.doNewIORef (cm, s)
                         -- now replace the render in the model
                         rnd <- R.mkRenderer $ do
                             (_, s') <- lift $ R.doReadIORef ref
                             disp s'
                         R.doModifyIORef' ref (\(cm', s') -> (cm' & F.componentRender .~ rnd, s'))
                         pure ref
                 )
     , F.Handler $ \ref a -> hdl (ref, Lens _2) a
     , F.Activator $ \ref exec -> act (ref, Lens _2) exec
     , F.Display $ \ref -> do
             (cm, _) <- lift $ R.doReadIORef ref
             R.lf (cm ^. F.component . to JE.toJS')
                 (cm ^. F.componentListeners)
                 [ ("key",  cm ^. F.componentKey . to (JE.toJS' . R.runKey))
                 , ("render", cm ^. F.componentRender . to (JE.toJS' . R.runRenderer))
                 ]
     )
