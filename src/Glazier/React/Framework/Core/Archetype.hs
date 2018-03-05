{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Framework.Core.Archetype where

import Control.Arrow
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import qualified Data.DList as DL
import Data.Generics.Product
import Data.IORef
import qualified Data.JSString as J
import qualified Data.Maybe.Esoteric as E
import Data.Semigroup
import qualified GHC.Generics as G
import qualified Glazier.Core as Z
import qualified Glazier.React as Z
import qualified Glazier.React.Framework.Core.Display as Z
import qualified Glazier.React.Framework.Core.Model as Z
import qualified Glazier.React.Framework.Core.Prototype as Z
import qualified JavaScript.Extras as JE

data Archetype m s c = Archetype
    { display' :: Z.Display m s ()
    , finalizer' :: Z.Finalizer m s
    , initializer' :: Z.Initializer m s c
    } deriving (G.Generic, Functor)

-- mapBuilder' :: (Z.Builder m i1 s i1 s -> Z.Builder m i2 s i2 s)
--     -> Archetype m i1 s c -> Archetype m i2 s c
-- mapBuilder' f p = let bld = builder' p in p { builder' = f bld }
-- infixl 4 `mapBuilder'` -- like <$>

modifyDisplay' :: (Z.Display m s () -> Z.Display m s ())
    -> Archetype m s c -> Archetype m s c
modifyDisplay' f p = let disp = display' p in p { display' = f disp }
infixl 4 `modifyDisplay'` -- like <$>

modifyFinalizer' :: (Z.Finalizer m s -> Z.Finalizer m s)
    -> Archetype m s c -> Archetype m s c
modifyFinalizer' f p = let fin = finalizer' p in p { finalizer' = f fin }
infixl 4 `modifyFinalizer'` -- like <$>

modifyInitializer' :: (Z.Initializer m s c1 -> Z.Initializer m s c2)
    -> Archetype m s c1 -> Archetype m s c2
modifyInitializer' f p = let ini = initializer' p in p { initializer' = f ini }
infixl 4 `modifyInitializer'` -- like <$>

-- mapHandler' :: (Z.Handler m s a1 b1 -> Z.Handler m s a2 b2)
--     -> Archetype m i s c a1 b1 -> Archetype m i s c a2 b2
-- mapHandler' f p = let hdl = handler' p in p { handler' = f hdl }
-- infixl 4 `mapHandler'` -- like <$>

toArchetypeBuilder :: Z.MonadReactor m
    => J.JSString
    -> Z.Builder m r s r' s'
    -> Z.Builder m r (IORef (Z.Frame s m)) r' (IORef (Z.Frame s m'))
toArchetypeBuilder n (Z.Builder (Z.MkReq mkReq, Z.MkSpec mkSpc)) = Z.Builder
        ( Z.MkReq (Z.doReadIORef >=> (mkReq . snd))
        , Z.MkSpec $ \i -> do
            -- tuple the original state with a ComponentPlan
            -- and wrap inside a IORef
            s <- mkSpc i
            cp <- Z.mkPlan n
            Z.doNewIORef (cp, s)
        )

toArchetypeHandler ::
    Z.SceneHandler m (Z.Frame s m) s a b -- ^ @v@ is no longer polymorphic
    -> Z.Handler m (IORef (Z.Frame s m)) a b
toArchetypeHandler hdl ref = hdl (Z.Obj ref id)

fromArchetypeHandler :: Z.MonadReactor m => Z.Handler m s a b -> Z.SceneHandler v s m a b
fromArchetypeHandler hdl (Z.Obj ref its) a = do
    obj <- lift $ Z.doReadIORef ref
    hdl (obj ^. its.Z.model) a

-- | NB. fromArchetype . toArchetype != id
toArchetype :: Z.MonadReactor m
    => Z.Prototype m (Z.Frame s m) s c -- ^ @v@ is no longer polymorphic
    -> Archetype m (IORef (Z.Frame s m)) c
toArchetype
    (Z.Prototype dis fin ini)
    = Archetype dis' fin' ini'
  where
    dis' ref = do
        (cp, _) <- lift $ Z.doReadIORef ref
        Z.leaf
            (DL.fromList $ E.keepMaybes [ ("updated", cp ^. field @"onUpdated")])
            (cp ^. field @"component".to JE.toJSR)
            (DL.fromList $ E.keepMaybes
                [ ("key", Just . JE.toJSR $ cp ^. field @"reactKey")
                , ("render", JE.toJSR <$> cp ^. field @"onRender")
                ])
    fin' ref = do
        (cp, s) <- Z.doReadIORef ref
        fin'' <- fin s
        pure (fin'' <> Z.disposeOnRemoved cp <> Z.disposeOnUpdated cp)
    ini' ref = do
        -- Run the Prototype's Initializer, saving the continuation result
        a <- ini (Z.Obj ref id)
        -- Now activate this archetype, passing the output
        fmap (const a) . lift $ do
            -- Now add our own Archetype activation
            (cp, _) <- Z.doReadIORef ref
            -- now replace the render and componentUpdated in the model if not already activated
            rnd <- case Z.onRender cp of
                        Just _ -> pure Nothing
                        Nothing -> fmap Just . Z.doMkRenderer $ do
                            s <- lift $ Z.doReadIORef ref
                            dis s
            upd <- case Z.onUpdated cp of
                        Just _ -> pure Nothing
                        Nothing -> fmap Just . Z.doMkCallback (const $ pure ()) . const $ do
                            (cp', _) <- Z.doReadIORef ref
                            Z.doModifyIORef' ref $ \s -> s
                                -- can't use '.~' with afterOnUpdated - causes type inference errors
                                & (Z.plan.field @"onceOnUpdated") `set'` pure mempty
                                & Z.plan.field @"disposeOnUpdated" .~ mempty
                            -- Now run things on every updated
                            Z.doDispose (Z.disposeOnUpdated cp')
                            Z.everyOnUpdated cp'
                            Z.onceOnUpdated cp'
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
                Just g -> Z.doModifyIORef' ref (first g)

    -- hdl' ref = hdl (Z.Obj ref id)

-- | NB. fromArchetype . toArchetype != id
fromArchetype :: Z.MonadReactor m
    => Archetype m s c
    -> Z.Prototype m v s c
fromArchetype (Archetype disp fin ini) = Z.Prototype
    (\(_, s) -> disp s)
    fin
    (\(Z.Obj ref its) -> do
        obj <- lift $ Z.doReadIORef ref
        ini (obj ^. its.Z.model))
    -- (\(Z.Obj ref its) a -> do
    --     obj <- lift $ Z.doReadIORef ref
    --     hdl (obj ^. its.Z.model) a)

fromArchetypeMaybe :: Z.MonadReactor m
    => Archetype m s c
    -> Z.Prototype m v (Maybe s) c
fromArchetypeMaybe (Archetype disp fin ini) = Z.Prototype
    (\(_, s) -> maybe (pure ()) disp s)
    (maybe (pure mempty) fin)
    (\(Z.Obj ref its) -> do
        obj <- lift $ Z.doReadIORef ref
        case obj ^. its.Z.model of
            Nothing -> ContT $ const $ pure ()
            Just s' -> ini s')
    -- (\(Z.Obj ref its) a -> do
    --     obj <- lift $ Z.doReadIORef ref
    --     hdl (obj ^. its.Z.model) a)

fromArchetypeMaybeHandler :: Z.MonadReactor m => Z.Handler m s a b -> Z.SceneHandler m v (Maybe s) a b
fromArchetypeMaybeHandler hdl (Z.Obj ref its) a = do
    obj <- lift $ Z.doReadIORef ref
    case obj ^. its.Z.model of
        Nothing -> ContT $ const $ pure ()
        Just s' -> hdl s' a
