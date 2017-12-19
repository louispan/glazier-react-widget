{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Framework.Archetype where

import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import Data.Generics.Product
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

newtype Archetype m p s a b x c = Archetype {
    runArchetype ::
           ( F.Display m s ()
           , F.Builder m p s p s
           , F.Handler m s a b
           -- activator contains other prerequisites
           -- of executor, and actions that need to be handled
           , F.Activator m s x c
           )
    }

-- | NB. fromArchetype . toArchetype != id
toArchetype :: (R.MonadReactor x m, x ~ R.Disposable ())
    => F.Prototype m (F.ComponentModel, s) p s p s a b x c
    -> Archetype m p (IORef (F.ComponentModel, s)) a b x c
toArchetype (F.Prototype ( F.Display disp
                         , F.Builder (F.MkPlan mkPlan, F.MkModel mkModel)
                         , F.Handler hdl
                         , F.Activator act
                         )) = Archetype
     ( F.Display $ \ref -> do
             (cm, _) <- lift $ R.doReadIORef ref
             R.lf (cm ^. field @"component".to JE.toJS')
                 mempty
                 [ ("key",  cm ^. field @"componentKey".to JE.toJS')
                 , ("render", cm ^. field @"componentRender".to JE.toJS')
                 , ("updated", cm ^. field @"componentUpdated".to JE.toJS')
                 ]
     , F.Builder ( F.MkPlan (R.doReadIORef >=> (mkPlan . snd))
                 , F.MkModel $ \p -> do
                         -- tuple the original state with a ComponentModel
                         -- and wrap inside a IORef
                         s <- mkModel p
                         -- create a ComponentModel with a dummy render and updated for now
                         cm <- F.ComponentModel
                                 <$> R.getComponent
                                 <*> pure mempty -- Disposables
                                 <*> pure (J.Callback J.nullRef)
                                 <*> R.mkReactKey
                                 <*> pure (R.Renderer (J.Callback J.nullRef))
                                 <*> pure 0
                         -- create the IORef
                         ref <- R.doNewIORef (cm, s)
                         -- now replace the render in the model
                         rnd <- R.mkRenderer $ do
                             (_, s') <- lift $ R.doReadIORef ref
                             disp s'
                         upd <- R.mkCallback (const $ pure ()) (const $ do
                                 (cm', _) <- R.doReadIORef ref
                                 let ds = cm' ^. field @"componentDisposable"
                                 case R.runDisposable ds of
                                     Nothing -> pure mempty -- ()
                                     Just ds' -> do
                                         R.doModifyIORef' ref (\(cm'', s') ->
                                             (cm'' & field @"componentDisposable" .~ mempty
                                             , s'))
                                         pure ds)
                                         -- ds')
                         R.doModifyIORef' ref (\(cm', s') ->
                                     ( cm' & field @"componentRender" .~ rnd
                                           & field @"componentUpdated" .~ upd
                                     , s'))
                         -- return the ioref
                         pure ref
                 )
     , F.Handler $ \ref a -> hdl (ref, Lens id) a
     , F.Activator $ \exec ref -> act exec (ref, Lens id)
     )

-- -- | NB. fromArchetype . toArchetype != id
-- fromArchetype :: R.MonadReactor m => Archetype m p s x y a b -> F.Prototype m v p s p s x y a b
-- fromArchetype (Archetype ( disp
--                          , bld
--                          , F.Activator act
--                          , F.Handler hdl
--                          )) = F.Prototype
--     ( disp
--     , bld
--     , F.Activator $ \exec (ref, Lens this) -> do
--             obj <- R.doReadIORef ref
--             act exec (obj ^. this._2)
--     , F.Handler $ \exec (ref, Lens this) a -> do
--             obj <- R.doReadIORef ref
--             hdl exec (obj ^. this._2) a
--     )

