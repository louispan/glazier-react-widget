{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Framework.Core.Archetype where

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
import qualified Glazier.React.Framework.Core.Activator as R
import qualified Glazier.React.Framework.Core.Builder as R
import qualified Glazier.React.Framework.Core.Display as R
import qualified Glazier.React.Framework.Core.Finalizer as R
import qualified Glazier.React.Framework.Core.Handler as R
import qualified Glazier.React.Framework.Core.Model as R
import qualified Glazier.React.Framework.Core.Obj as R
import qualified Glazier.React.Framework.Core.Prototype as R
import qualified JavaScript.Extras as JE

data Archetype m i s c a b = Archetype
    { builder' :: R.Builder m i s i s
    , display' :: R.Display m s ()
    , finalizer' :: R.Finalizer m s
    , activator' :: R.Activator m s c
    , handler' :: R.Handler m s a b
    } deriving (G.Generic)

mapBuilder' :: (R.Builder m i1 s i1 s -> R.Builder m i2 s i2 s)
    -> Archetype m i1 s c a b -> Archetype m i2 s c a b
mapBuilder' f p = let bld = builder' p in p { builder' = f bld }
infixl 4 `mapBuilder'` -- like <$>

mapDisplay' :: (R.Display m s () -> R.Display m s ())
    -> Archetype m i s c a b -> Archetype m i s c a b
mapDisplay' f p = let disp = display' p in p { display' = f disp }
infixl 4 `mapDisplay'` -- like <$>

mapFinalizer' :: (R.Finalizer m s -> R.Finalizer m s)
    -> Archetype m i s c a b -> Archetype m i s c a b
mapFinalizer' f p = let fin = finalizer' p in p { finalizer' = f fin }
infixl 4 `mapFinalizer'` -- like <$>

mapActivator' :: (R.Activator m s c1 -> R.Activator m s c2)
    -> Archetype m i s c1 a b -> Archetype m i s c2 a b
mapActivator' f p = let act = activator' p in p { activator' = f act }
infixl 4 `mapActivator'` -- like <$>

mapHandler' :: (R.Handler m s a1 b1 -> R.Handler m s a2 b2)
    -> Archetype m i s c a1 b1 -> Archetype m i s c a2 b2
mapHandler' f p = let hdl = handler' p in p { handler' = f hdl }
infixl 4 `mapHandler'` -- like <$>

-- | NB. fromArchetype . toArchetype != id
toArchetype :: R.MonadReactor m
    => J.JSString
    -> R.Prototype m (R.Frame m s) i s i s c a b
    -> Archetype m i (IORef (R.Frame m s)) c a b
toArchetype n (R.Prototype
    (R.Builder (R.MkInfo mkInf, R.MkSpec mkSpc))
    dis
    fin
    act
    hdl
    )
    = Archetype bld' dis' fin' act' hdl'
  where
    bld' = R.Builder
        ( R.MkInfo (R.doReadIORef >=> (mkInf . snd))
        , R.MkSpec $ \i -> do
            -- tuple the original state with a ComponentPlan
            -- and wrap inside a IORef
            s <- mkSpc i
            cp <- R.mkPlan n
            R.doNewIORef (cp, s)
        )
    dis' ref = do
        (cp, _) <- lift $ R.doReadIORef ref
        R.leaf
            (JE.justSnds [ ("updated", cp ^. field @"onUpdated")])
            (cp ^. field @"component".to JE.toJS')
            (JE.justSnds
                [ ("key", Just . JE.toJS' $ cp ^. field @"reactKey")
                , ("render", JE.toJS' <$> cp ^. field @"onRender")
                ])
    fin' ref = do
        (cp, s) <- R.doReadIORef ref
        fin'' <- fin s
        pure (fin'' <> R.disposeOnRemoved cp <> R.disposeOnUpdated cp)
    act' ref = do
        -- Run the Prototype's activator, saving the continuation result
        a <- act (R.Obj ref id)
        -- Now activate this archetype, passing the output
        fmap (const a) . lift $ do
            -- Now add our own Archetype activation
            (cp, _) <- R.doReadIORef ref
            -- now replace the render and componentUpdated in the model if not already activated
            rnd <- case R.onRender cp of
                        Just _ -> pure Nothing
                        Nothing -> fmap Just . R.doMkRenderer $ do
                            s <- lift $ R.doReadIORef ref
                            dis s
            upd <- case R.onUpdated cp of
                        Just _ -> pure Nothing
                        Nothing -> fmap Just . R.doMkCallback (const $ pure ()) . const $ do
                            (cp', _) <- R.doReadIORef ref
                            R.doModifyIORef' ref $ \s -> s
                                -- can't use '.~' with afterOnUpdated - causes type inference errors
                                & (R.plan.field @"afterOnUpdated") `set'` pure mempty
                                & R.plan.field @"disposeOnUpdated" .~ mempty
                            R.doDispose (R.disposeOnUpdated cp')
                            R.afterOnUpdated cp'
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
                Just g -> R.doModifyIORef' ref (first g)
    hdl' ref = hdl (R.Obj ref id)

-- | NB. fromArchetype . toArchetype != id
fromArchetype :: R.MonadReactor m
    => Archetype m i s c a b
    -> R.Prototype m v i s i s c a b
fromArchetype (Archetype
    bld
    dis
    fin
    act
    hdl
    )
    = R.Prototype
    bld
    (\(_, s) -> dis s)
    fin
    (\(R.Obj ref its) -> do
        obj <- lift $ R.doReadIORef ref
        act (obj ^. its.R.model))
    (\(R.Obj ref its) a -> do
        obj <- lift $ R.doReadIORef ref
        hdl (obj ^. its.R.model) a)
