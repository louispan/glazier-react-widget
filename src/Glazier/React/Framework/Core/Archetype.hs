{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.React.Framework.Core.Archetype where

import Control.Arrow
import Control.Lens
import Control.Monad
import Control.Monad.Reader
import qualified Data.DList as DL
import Data.Generics.Product
import qualified Data.JSString as J
import qualified Data.Maybe.Esoteric as E
import Data.Semigroup
import Data.Semigroup.Applicative
import qualified GHC.Generics as G
import Glazier.Core
import Glazier.React
import Glazier.React.Framework.Core.Display
import Glazier.React.Framework.Core.Model
import Glazier.React.Framework.Core.Prototype
import qualified JavaScript.Extras as JE

data Archetype s m c = Archetype
    { display' :: Display s m ()
    , finalizer' :: Finalizer s m
    , initializer' :: MethodT s m c
    } deriving (G.Generic, Functor)

_display' :: Lens' (Archetype s m c) (Display s m ())
_display' = field @"display'"

_finalizer' :: Lens' (Archetype s m c) (Finalizer s m)
_finalizer' = field @"finalizer'"

_initializer' :: Lens (Archetype s m c) (Archetype s m c')
    (MethodT s m c) (MethodT s m c')
_initializer' = field @"initializer'"

withArchetype :: Monad m =>
    (MethodT s m c1 -> MethodT s m c2 -> MethodT s m c3)
    -> Archetype s m c1 -> Archetype s m c2 ->  Archetype s m c3
withArchetype f (Archetype dis1 fin1 ini1) (Archetype dis2 fin2 ini2) =
    Archetype
    (dis1 <> dis2)
    (fin1 <> fin2)
    (f ini1 ini2)

toArchetypeBuilder :: MonadReactor m
    => J.JSString
    -> Builder r s m r' s'
    -> Builder r (Specimen m s) m r' (Specimen m s')
toArchetypeBuilder n (Builder mkReq mkSpc) =
    Builder mkReq' mkSpc'
  where
    mkReq' = doReadIORef >=> (mkReq . model)
    mkSpc' r = do
        -- tuple the original state with a ComponentPlan
        -- and wrap inside a IORef
        s <- mkSpc r
        cp <- mkPlan n
        doNewIORef (Frame cp s)

toArchetypeHandler ::
    (a -> MethodT (Scene (Frame m s) m s) m b) -- ^ @v@ is no longer polymorphic
    -> a -> MethodT (Specimen m s) m b
toArchetypeHandler hdl a = do
    r <- ask
    magnify (to . const $ Obj r id) (hdl a)

fromArchetypeHandler :: MonadReactor m =>
    (a -> MethodT s m b) ->
    a -> MethodT (Scene p m s) m b
fromArchetypeHandler hdl a = do
    Obj{..} <- ask
    me <- lift $ lift $ doReadIORef self
    magnify (to . const $ me ^. my._model) (hdl a)

-- | NB. fromArchetype . toArchetype != id
toArchetype :: MonadReactor m
    => Prototype (Frame m s) s m c -- ^ @v@ is no longer polymorphic
    -> Archetype (Specimen m s) m c
toArchetype (Prototype dis fin ini) = Archetype dis' fin' ini'
  where
    dis' ref = do
        Frame cp _ <- lift $ doReadIORef ref
        leaf
            (DL.fromList $ E.keepMaybes [ ("updated", cp ^. _onUpdated)])
            (cp ^. _component.to JE.toJSR)
            (DL.fromList $ E.keepMaybes
                [ ("key", Just . JE.toJSR $ cp ^. _reactKey)
                , ("render", JE.toJSR <$> cp ^. _onRender)
                ])
    fin' ref = Ap $ do
        Frame cp s <- doReadIORef ref
        fin'' <- getAp $ fin s
        pure (fin'' <> disposeOnRemoved cp <> disposeOnUpdated cp)
    ini' = do
        ref <- ask
        -- Run the Prototype's Initializer, saving the continuation result
        a <- magnify (to . const $ Obj ref id) ini
        -- Now activate this archetype, passing the output
        fmap (const a) . lift $ lift $ do
            -- Now add our own Archetype activation
            Frame cp _ <- doReadIORef ref
            -- now replace the render and componentUpdated in the model if not already activated
            rnd <- case onRender cp of
                        Just _ -> pure Nothing
                        Nothing -> fmap Just . doMkRenderer $ do
                            s <- lift $ doReadIORef ref
                            dis s
            upd <- case onUpdated cp of
                        Just _ -> pure Nothing
                        Nothing -> fmap Just . doMkCallback (const $ pure ()) . const $ do
                            Frame cp' _ <- doReadIORef ref
                            doModifyIORef' ref $
                                -- can't use '.~' with afterOnUpdated - causes type inference errors
                                (_plan._onceOnUpdated) `set'` pure mempty
                                >>> _plan._disposeOnUpdated .~ mempty
                            -- Now run things on every updated
                            doDispose (disposeOnUpdated cp')
                            everyOnUpdated cp'
                            onceOnUpdated cp'
            let rnd' = (\(d, cb)
                        -> _onRender .~ Just cb
                        >>> _disposeOnRemoved %~ (<> d)
                        ) <$> rnd
                upd' = (\(d, cb)
                        -> _onUpdated .~ Just cb
                        >>> _disposeOnRemoved %~ (<> d)
                        ) <$> upd
                mf = case (rnd', upd') of
                        (Nothing, x) -> x
                        (x, Nothing) -> x
                        (Just x, Just y) -> Just (y . x)
            case mf of
                Nothing -> pure ()
                Just g -> doModifyIORef' ref (_plan %~ g)

-- | NB. fromArchetype . toArchetype != id
fromArchetype :: MonadReactor m
    => Archetype s m c
    -> Prototype p s m c
fromArchetype (Archetype disp fin ini) = Prototype
    (magnify _model disp)
    fin
    (do
        Obj{..} <- ask
        me <- lift $ lift $ doReadIORef self
        magnify (to . const $ (me ^. my._model)) ini)

fromMaybeArchetype :: MonadReactor m
    => Archetype s m c
    -> Prototype p (Maybe s) m c
fromMaybeArchetype (Archetype disp fin ini) = Prototype
    -- (\(_, s) -> maybe (pure ()) disp s)
    (magnify (_model._Just) disp)
    (magnify _Just fin)
    (do
        Obj{..} <- ask
        me <- lift . lift $ doReadIORef self
        case me ^. my._model of
            Nothing -> mempty
            Just s' -> magnify (to . const $ s') ini)

fromMaybeArchetypeHandler :: MonadReactor m =>
    (a -> MethodT s m b) -> (a -> MethodT (Scene p m (Maybe s)) m b)
fromMaybeArchetypeHandler hdl a = do
    Obj{..} <- ask
    me <- lift $ lift $ doReadIORef self
    case me ^. my._model of
        Nothing -> mempty
        Just s' -> magnify (to . const $ s') (hdl a)
