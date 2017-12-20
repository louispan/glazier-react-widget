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
import Data.Diverse.Lens
import qualified Data.DList as DL
import Data.Generics.Product
import Data.IORef
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Activator as F
import qualified Glazier.React.Framework.Builder as F
import qualified Glazier.React.Framework.Core as F
import qualified Glazier.React.Framework.Display as F
import qualified Glazier.React.Framework.Executor as F
import qualified Glazier.React.Framework.Handler as F
import qualified Glazier.React.Framework.Prototype as F
import qualified GHCJS.Foreign.Callback.Internal as J
import qualified GHCJS.Types as J
import qualified JavaScript.Extras as JE

newtype Archetype m p s x c a b = Archetype {
    runArchetype ::
           ( F.Display m s ()
           , F.Builder m p s p s
           , F.Executor m s x c a b
           )
    }

-- | NB. fromArchetype . toArchetype != id
toArchetype :: (R.MonadReactor x m, AsFacet (R.Disposable ()) x)
    => F.Prototype m (F.ComponentModel, s) p s p s x c a b
    -> Archetype m p (IORef (F.ComponentModel, s)) x c a b
toArchetype (F.Prototype ( F.Display disp
                         , F.Builder (F.MkPlan mkPlan, F.MkModel mkModel)
                         , F.Executor exec
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
                                     Nothing -> pure DL.empty
                                     Just _ -> do
                                         R.doModifyIORef' ref (\(cm'', s') ->
                                             (cm'' & field @"componentDisposable" .~ mempty
                                             , s'))
                                         pure $ DL.singleton $ review facet ds)
                                         -- ds')
                         R.doModifyIORef' ref (\(cm', s') ->
                                     ( cm' & field @"componentRender" .~ rnd
                                           & field @"componentUpdated" .~ upd
                                     , s'))
                         -- return the ioref
                         pure ref
                 )
     , F.Executor $ \k -> let (F.Activator act, F.Handler hdl) = exec k
                          in ( F.Activator $ \ref -> act (ref, Lens id)
                             , F.Handler $ \ref a -> hdl (ref, Lens id) a
                             )
     )

-- | NB. fromArchetype . toArchetype != id
fromArchetype :: R.MonadReactor x m => Archetype m p s x c a b -> F.Prototype m v p s p s x c a b
fromArchetype (Archetype ( disp
                         , bld
                         , F.Executor exec
                         -- , F.Handler hdl
                         -- , F.Activator act
                         )) = F.Prototype
    ( disp
    , bld
    , F.Executor $ \k -> let (F.Activator act, F.Handler hdl) = exec k
                          in ( F.Activator $ \(ref, Lens this) -> do
                                     obj <- R.doReadIORef ref
                                     act (obj ^. this._2)
                             , F.Handler $ \(ref, Lens this) a -> do
                                     obj <- R.doReadIORef ref
                                     hdl (obj ^. this._2) a
                             )
    )
