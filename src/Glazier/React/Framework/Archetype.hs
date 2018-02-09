{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Framework.Archetype where

import Control.Arrow
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import Data.Generics.Product
import Data.IORef
import qualified Data.JSString as J
import Data.Semigroup
import qualified GHC.Generics as G
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Activator as F
import qualified Glazier.React.Framework.Builder as F
import qualified Glazier.React.Framework.Core as F
import qualified Glazier.React.Framework.Display as F
import qualified Glazier.React.Framework.Executor as F
import qualified Glazier.React.Framework.Finalizer as F
import qualified Glazier.React.Framework.Handler as F
import qualified Glazier.React.Framework.Object as F
import qualified Glazier.React.Framework.Prototype as F
import qualified JavaScript.Extras as JE

data Archetype x m i s y z a b = Archetype
    { builder :: F.Builder m i s i s
    , display :: F.Display m s ()
    , finalizer :: F.Finalizer m s
    , activator :: F.ExActivator x m s y
    , handler :: F.ExHandler x m s z a b
    } deriving (G.Generic)

-- | NB. fromArchetype . toArchetype != id
toArchetype :: (R.MonadReactor x m)
    => J.JSString -> F.Prototype x m (F.Plan x m, s) i s i s y z a b
    -> Archetype x m i (IORef (F.Plan x m, s)) y z a b
toArchetype n (F.Prototype
    (F.Builder (F.MkInfo mkInf, F.MkModel mkMdl))
    (F.Display dis)
    (F.Finalizer fin)
    (F.Executor xact)
    (F.Executor xhdl))
    = Archetype
    (F.Builder
        ( F.MkInfo (R.doReadIORef >=> (mkInf . snd))
        , F.MkModel $ \i -> do
            -- tuple the original state with a ComponentPlan
            -- and wrap inside a IORef
            s <- mkMdl i
            cp <- F.mkPlan n
            R.doNewIORef (cp, s)))
    (F.Display $ \ref -> do
            (cp, _) <- lift $ R.doReadIORef ref
            R.leaf (cp ^. field @"component".to JE.toJS')
                (JE.justSnds
                    [ ("updated", cp ^. field @"onUpdated")
                    ]
                )
                (JE.justSnds
                    [ ("key", Just . JE.toJS' $ cp ^. field @"key")
                    , ("render", JE.toJS' <$> cp ^. field @"onRender")
                    ]))
    (F.Finalizer $ \ref -> do
            (cp, s) <- R.doReadIORef ref
            fin' <- fin s
            pure (fin' <> F.disposeOnRemoved cp <> F.disposeOnUpdated cp))
    (F.Executor $ \k ->
        let F.Activator act = xact k
        in F.Activator $ \ref -> do
            act (F.Object ref (Lens id))
            (cp, _) <- R.doReadIORef ref
            -- now replace the render and componentUpdated in the model if not already activated
            rnd <- case F.onRender cp of
                        Just _ -> pure Nothing
                        Nothing -> fmap Just . R.mkRenderer $ do
                            s <- lift $ R.doReadIORef ref
                            dis s
            upd <- case F.onUpdated cp of
                        Just _ -> pure Nothing
                        Nothing -> fmap Just . R.mkCallback (const $ pure ()) . const $ do
                            (cp', _) <- R.doReadIORef ref
                            R.doModifyIORef' ref $ \(cp'', s') ->
                                (cp'' & field @"doOnUpdated" `set'` pure mempty
                                        & field @"disposeOnUpdated" .~ mempty
                                , s')
                            R.dispose (F.disposeOnUpdated cp')
                            F.doOnUpdated cp'
            let rnd' = (\(d, cb) cp' -> cp' & field @"onRender" .~ Just cb
                                            & field @"disposeOnRemoved" %~ (<> d)
                        ) <$> rnd
                upd' = (\(d, cb) cp' -> cp' & field @"onUpdated" .~ Just cb
                                            & field @"disposeOnRemoved" %~ (<> d)
                        ) <$> upd
                mf = case (rnd', upd') of
                        (Nothing, x) -> x
                        (x, Nothing) -> x
                        (Just x, Just y) -> Just (y . x)
            case mf of
                Nothing -> pure ()
                Just g -> R.doModifyIORef' ref (first g))
    (F.Executor $ \k -> let F.Handler hdl = xhdl k
                        in F.Handler $ \ref a -> hdl (F.Object ref (Lens id)) a)

-- | NB. fromArchetype . toArchetype != id
fromArchetype :: R.MonadReactor x m
    => Archetype x m i s y z a b
    -> F.Prototype x m v i s i s y z a b
fromArchetype (Archetype
    bld
    (F.Display dis)
    fin
    (F.Executor xact)
    (F.Executor xhdl))
    = F.Prototype
    bld
    (F.Display $ \(_, s) -> dis s)
    fin
    (F.Executor $ \k -> let F.Activator act = xact k
                        in F.Activator $ \(F.Object ref (Lens this)) -> do
                                obj <- R.doReadIORef ref
                                act (obj ^. this._2))
    (F.Executor $ \k -> let F.Handler hdl = xhdl k
                        in F.Handler $ \(F.Object ref (Lens this)) a -> do
                                obj <- R.doReadIORef ref
                                hdl (obj ^. this._2) a)
