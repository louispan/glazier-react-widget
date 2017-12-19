{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE TypeOperators #-}
-- {-# LANGUAGE NoMonomorphismRestriction #-}

module Glazier.React.Prototypes.Listing where

import Control.Lens
import qualified Data.DList as DL
import Data.Diverse.Lens
import Data.Foldable
import Data.Generics.Product
import Data.IORef
import qualified Data.List.NonEmpty as NE
-- import qualified Data.JSString as J
import Data.Semigroup
-- import Data.Maybe
import qualified Data.Map.Strict as M
import qualified Glazier.React as R
import qualified Glazier.React.Framework as F
import qualified Glazier.React.Commands as C
import qualified JavaScript.Extras as JE
-- import qualified Pipes.Concurrent as PC

-- | Internal: Make a key that will fit in between the two provided keys,
-- Except when the inputs are equal, then it will return the same key.
betweenIdx' :: [Int] -> [Int] -> [Int]
betweenIdx' [] [] = []
betweenIdx' [] (y : _) = [y - 1]
betweenIdx' (x : _) [] = [x - 1]
betweenIdx' (x : xs) (y : ys) =
    case compare x y of
        LT -> x : (largerIdx' xs)
        GT -> y : (largerIdx' ys)
        EQ -> x : (betweenIdx' xs ys)

-- | Make a key that will fit in between the two provided keys,
-- Except when the inputs are equal, then it will return the same key.
betweenIdx :: NE.NonEmpty Int -> NE.NonEmpty Int -> NE.NonEmpty Int
betweenIdx (x NE.:| xs) (y NE.:| ys) =
    case compare x y of
        LT -> x NE.:| (largerIdx' xs)
        GT -> y NE.:| (largerIdx' ys)
        EQ -> x NE.:| (betweenIdx' xs ys)

-- | Internal: Create a key larger than the input key.
-- NB. It does not create the smallest key that is larger.
largerIdx' :: [Int] -> [Int]
largerIdx' [] = [1]
largerIdx' (a : _) = [a + 1]

-- | Create a key larger than the input key.
-- NB. It does not create the smallest key that is larger.
largerIdx :: NE.NonEmpty Int -> NE.NonEmpty Int
largerIdx (a NE.:| _) = (a + 1) NE.:| []

-- | Create a key smaller than the input key.
-- NB. It does not create the largest key that is smaller.
smallerIdx :: NE.NonEmpty Int -> NE.NonEmpty Int
smallerIdx (a NE.:| _) = (a - 1) NE.:| []

-- | A listing is actually a map so that we allow for fast insert/deleting
-- and also be able to reorder the elements.
type Listing a = M.Map (NE.NonEmpty Int) a

-- | List specific actions
data ListingDeleteItem = ListingDeleteItem (NE.NonEmpty Int)
data ListingMoveItem = ListingMoveItem (NE.NonEmpty Int) (NE.NonEmpty Int)
data ListingInsertItem s = ListingInsertItem (NE.NonEmpty Int) s
data ListingConsItem s = ListingConsItem s
data ListingSnocItem s = ListingSnocItem s
data ListingMakeItem p f = ListingMakeItem p f

newtype ListingNewItemAction s = ListingNewItemAction {
    runListingNewItemAction :: Which '[ ListingInsertItem s
                                      , ListingConsItem s
                                      , ListingSnocItem s
                                      ] }

newtype ListingAction p s = ListingAction {
    runListingAction :: Which '[ ListingNewItemAction s
                               , ListingMakeItem p (s -> ListingNewItemAction s)
                               , ListingDeleteItem
                               , ListingMoveItem
                               ] }

newtype ListingItemProperties = ListingItemProperties {
    runListingItemProperties :: DL.DList JE.Property
    }

-- | This version drops the original item handlers.
-- The listing activator doesn't require C.Rerender. C.Rerender is only required if the listing handler is used in a parent activator.
listing ::
    ( R.MonadReactor x m
    , R.Dispose s
    , HasItem' (Listing p) ps
    , HasItem' (Listing s) ss
    , HasItem' (DL.DList JE.Property) ss
    , HasItem' ListingItemProperties ss
    , HasItem' (DL.DList R.Listener) ss
    )
  => F.Archetype m p s a b x x -> F.Prototype m v ps ss
    (Many '[Listing p])
    (Many '[Listing s])
    (Which '[ListingAction p s])
    (Which '[C.Rerender])
    x
    x
listing (F.Archetype (disp, bld@(F.Builder (_, mkMdl)), _, act)) = F.Prototype
    ( listingDisplay disp
    , F.toItemBuilder (listingBuilder bld)
    , F.viaModel (alongside id item') (F.toFacetedHandler (listingRefHandler mkMdl act))
    , F.viaModel (alongside id item') (listingActivator act)
    )

onListingDeleteItem :: (R.MonadReactor x m, R.Dispose s)
  => IORef v
  -> Lens' v (F.ComponentModel, Listing s)
  -> ListingDeleteItem
  -> m (DL.DList C.Rerender)
onListingDeleteItem ref this (ListingDeleteItem k) = do
       R.doModifyIORef' ref $ \obj ->
            let mi = M.lookup k (obj ^. this._2)
            in obj & (this._2 %~ M.delete k)
                   . (this._1.field @"componentDisposable" %~ (<> R.dispose mi))
       DL.singleton <$> C.mkRerender ref (this._1)

-- | Move an item from one key to another
-- If the Listing at the old key didn't exist and an item at the new key
-- already exist, this will delete the item at the new key.
onListingMoveItem :: (R.MonadReactor x m, R.Dispose s)
  => IORef v
  -> Lens' v (F.ComponentModel, Listing s)
  -> ListingMoveItem
  -> m (DL.DList C.Rerender)
onListingMoveItem ref this (ListingMoveItem oldK newK) = do
       R.doModifyIORef' ref $ \obj ->
            let mi = M.lookup newK (obj ^. this._2)
                mj = M.lookup oldK (obj ^. this._2)
            in case mj of
                Nothing -> obj & (this._1.field @"componentDisposable" %~ (<> R.dispose mi))
                Just i -> obj & (this._2 %~ M.insert newK i)
                   . (this._1.field @"componentDisposable" %~ (<> R.dispose mi))
       DL.singleton <$> C.mkRerender ref (this._1)

onListingInsertItem :: (R.MonadReactor x m, R.Dispose s)
  => IORef v
  -> Lens' v (F.ComponentModel, Listing s)
  -> ListingInsertItem s
  -> m (DL.DList C.Rerender)
onListingInsertItem ref this (ListingInsertItem k s) = do
       R.doModifyIORef' ref $ \obj ->
            let mi = M.lookup k (obj ^. this._2)
            in obj & (this._2 %~ M.insert k s)
                   . (this._1.field @"componentDisposable" %~ (<> R.dispose mi))
       DL.singleton <$> C.mkRerender ref (this._1)

onListingConsItem :: (R.MonadReactor x m, R.Dispose s)
  => IORef v
  -> Lens' v (F.ComponentModel, Listing s)
  -> ListingConsItem s
  -> m (DL.DList C.Rerender)
onListingConsItem ref this (ListingConsItem s) = do
       R.doModifyIORef' ref $ \obj ->
            let xs = M.toAscList (obj ^. this._2)
            in case xs of
                [] -> obj & (this._2 .~ M.singleton (0 NE.:| []) s)
                ((k, _) : _) -> obj & (this._2 %~ M.insert (smallerIdx k) s)
       DL.singleton <$> C.mkRerender ref (this._1)

onListingSnocItem :: (R.MonadReactor x m, R.Dispose s)
  => IORef v
  -> Lens' v (F.ComponentModel, Listing s)
  -> ListingSnocItem s
  -> m (DL.DList C.Rerender)
onListingSnocItem ref this (ListingSnocItem s) = do
       R.doModifyIORef' ref $ \obj ->
            let xs = M.toDescList (obj ^. this._2)
            in case xs of
                [] -> obj & (this._2 .~ M.singleton (0 NE.:| []) s)
                ((k, _) : _) -> obj & (this._2 %~ M.insert (largerIdx k) s)
       DL.singleton <$> C.mkRerender ref (this._1)

-- | Handler for ListingAction
listingNewItemRefHandler ::
    forall x m s v. ( R.MonadReactor x m
    , R.Dispose s
    )
    => F.RefHandler m v (F.ComponentModel, Listing s) (ListingNewItemAction s) C.Rerender
listingNewItemRefHandler = F.Handler $ \(ref, Lens this) (ListingNewItemAction a) ->
    switch a . cases $
        (onListingInsertItem @x @m ref this)
     ./ (onListingConsItem @x @m ref this)
     ./ (onListingSnocItem @x @m ref this)
     ./ nil

onListingMakeItem :: forall x m p s v. (R.MonadReactor x m, R.Dispose s)
  => F.MkModel m p s
  -> F.Activator m s x x
  -> IORef v
  -> Lens' v (F.ComponentModel, Listing s)
  -> ListingMakeItem p (s -> ListingNewItemAction s)
  -> m (DL.DList C.Rerender)
onListingMakeItem mkMdl act ref this (ListingMakeItem p f) = do
    s <- (F.runMkModel mkMdl) p
    (F.runActivator act) (F.Executor $ pure) s
    (F.runHandler listingNewItemRefHandler) (ref, Lens this) (f s)

-- | Handler for ListingAction
listingRefHandler ::
    forall m p s v x. ( R.MonadReactor x m
    , R.Dispose s
    )
    => F.MkModel m p s
    -> F.Activator m s x x
    -> F.RefHandler m v (F.ComponentModel, Listing s) (ListingAction p s) C.Rerender
listingRefHandler mkMdl act = F.Handler $ \v@(ref, Lens this) (ListingAction a) ->
    switch a . cases $
        ((F.runHandler (listingNewItemRefHandler @x @m @s)) v)
     ./ (onListingMakeItem @x @m @p @s mkMdl act ref this)
     ./ (onListingDeleteItem @x @m ref this)
     ./ (onListingMoveItem @x @m ref this)
     ./ nil

-- | lift a handler for a single widget into a handler of a list of widgets
-- where the input is broadcast to all the items in the list and the results DList'ed together.
listingBroadcastRefHandler
    ::
    ( R.MonadReactor x m
    )
    => F.Handler m s a b
    -> F.RefHandler m v (F.ComponentModel, Listing s) a b
listingBroadcastRefHandler (F.Handler hdl) = F.Handler $ \(ref, Lens this) a -> do
    obj <- R.doReadIORef ref
    ys <- traverse (\x -> hdl x a) (obj ^. this._2)
    pure $ fold ys

-- -- | Converts a builder with a plan of @[a]@ to a plan of @Listing a@
-- toListingBuilder
--     :: Applicative m
--     => F.Builder m [p] s [p'] s'
--     -> F.Builder m (Listing p) s (Listing p') s'
-- toListingBuilder = F.dimapPlan toList (M.fromAscList . zip idxs)
--   where
--     idxs = (\x -> (x NE.:| [])) <$> [0..]

listingBuilder
    :: (Applicative m)
    => F.Builder m p s p' s'
    -> F.Builder m (Listing p) (Listing s) (Listing p') (Listing s')
listingBuilder (F.Builder (F.MkPlan mkPln, F.MkModel mkMdl)) =
    F.Builder (F.MkPlan mkPln', F.MkModel mkMdl')
  where
    mkPln' ss = traverse mkPln ss
    mkMdl' ps = traverse mkMdl ps

listingDisplay
    :: forall m x s ss.
    ( R.MonadReactor x m
    , HasItem' (Listing s) ss
    , HasItem' (DL.DList JE.Property) ss
    , HasItem' ListingItemProperties ss
    , HasItem' (DL.DList R.Listener) ss
    )
    => F.Display m s ()
    -> F.Display m ss ()
listingDisplay (F.Display disp) = F.Display $ \ss ->
    let xs = ss ^. (item' @(Listing s))
        xs' = toLi <$> xs
        toLi s = R.bh "li"
                 []
                 (DL.toList . runListingItemProperties $ view (item' @ListingItemProperties) ss)
                 (disp s)
    in R.bh "ul"
        (DL.toList $ view (item' @(DL.DList R.Listener)) ss)
        (DL.toList $ view (item' @(DL.DList JE.Property)) ss)
        (mconcat $ (snd <$> M.toList xs'))

broadcastlistingActivator
    :: R.MonadReactor x m
    => F.Activator m s x c
    -> F.RefActivator m v (F.ComponentModel, Listing s) x c
broadcastlistingActivator (F.Activator act) = F.Activator $ \exec (ref, Lens this) -> do
    obj <- R.doReadIORef ref
    traverse_ (\s -> act exec s) (obj ^. this._2)

listingActivator
    :: R.MonadReactor x m
    => F.Activator m s x c
    -> F.RefActivator m v (F.ComponentModel, Listing s) x c
listingActivator (F.Activator act) = F.Activator $ \exec (ref, Lens this) -> do
    obj <- R.doReadIORef ref
    traverse_ (\s -> act exec s) (obj ^. this._2)

-- listActivator
--     :: forall deleteListItem r s specs a acts' acts c cmds.
--     ( UniqueMember (S.Seq (IORef s)) specs
--     , R.Dispose s
--     , UniqueMember deleteListItem acts'
--     , UniqueMember C.Rerender cmds
--     , UniqueMembers '[deleteListItem] a -- Redundant constraint: but used to make sure the Archetype triggers this action
--     , Reinterpret' (Complement a '[deleteListItem]) acts'
--     , Diversify acts (Complement a '[deleteListItem]))
--     => Proxy deleteListItem -> F.Archetype r s a acts' c cmds
--     -> F.Activator (F.Design specs) (Complement a '[deleteListItem]) acts (SnocUnique c C.Rerender) cmds
-- listActivator _ (F.Archetype (_ , _, _, _, _, activateEnt)) =
--     F.Activator (Proxy, Proxy, activateDesign)
--   where
--     activateDesign
--         :: F.Executor' cmds
--          -> F.Handler' (F.Design specs) acts cmds
--          -> IORef (F.Design specs)
--          -> MaybeT (F R.Reactor) (STM ())
--     activateDesign exec externalHdl this = do
--         this' <- lift . R.doSTM $ readIORef this
--         let vs = this' ^. (F.specifications . item @(S.Seq (IORef s)))
--             -- internalHdl :: IORef s -> Which acts -> MaybeT STM (DL.DList (Which cmds))
--             -- internalHdl _ = hdl this
--             internalHdl :: IORef s -> Which acts' -> MaybeT STM (DL.DList (Which cmds))
--             internalHdl v a = F.getHandler (deleteListItemHandler (Proxy @deleteListItem) v) this a
--             -- any actions required by @a@, but not handled by h, must be handled by externally provided handler
--             -- NB. The type of Which is changed: given acts', use externalHdl which uses acts
--             externalToInternalHdl :: IORef s -> Which acts' -> MaybeT STM (DL.DList (Which cmds))
--             externalToInternalHdl _ a =  case reinterpret' @(Complement a '[deleteListItem]) @acts' a of
--                 Nothing -> empty
--                 Just a' -> externalHdl this (diversify @acts a')
--             internalHdls v' a = internalHdl v' a <|> externalToInternalHdl v' a
--         foldl' (>>) (pure ()) <$> traverse (activateEnt exec internalHdls) vs

-- deleteListItem'
--     :: (R.Dispose s, UniqueMember (S.Seq (IORef s)) specs)
--     => IORef s -> MaybeT (StateT (F.Design specs) STM) C.Rerender
-- deleteListItem' s = do
--     ls <- use (F.specifications . item)
--     let (as, bs) = S.breakl (/= s) ls
--         (x, cs) = case S.viewl bs of
--             S.EmptyL -> (Nothing, as)
--             s' S.:< bs' -> (Just s', as S.>< bs')
--     case x of
--         Nothing -> empty
--         Just x' -> F.queueDisposable x'
--     (F.specifications . item) .= cs
--     F.rerender

-- deleteListItemHandler
--     :: ( R.Dispose s
--        , UniqueMember (S.Seq (IORef s)) specs
--        , UniqueMember deleteListItem acts
--        , UniqueMember C.Rerender cmds
--        )
--     => Proxy deleteListItem -> IORef s
--     -> F.Handler (F.Design specs) '[deleteListItem] acts '[C.Rerender] cmds
-- deleteListItemHandler _ s = F.stateHandler (\_ _ -> deleteListItem' s)


-- wack
--     :: UniqueMember (S.Seq (TMVar s)) specs
--     => TMVar v' -> Lens' v' (F.Design specs) -> F.Handler v s '[DestroyListItem] a
-- wack v' l' = F.Handler (Proxy, go)
--   where
--     go = undefined


-- wock :: (


-- -- TODO: use a Prism' a (ListAction o s) to get list actions from items to be wrapped in lists
-- listItemGadget'
--     :: forall m o s a c e dtls plns.
--        F.Archetype m o s a c e
--     -> PC.Output (ListItemAction s a)
--     -> G.GadgetT (ListItemAction s a) (F.Design dtls plns) STM (D.DList c)
-- listItemGadget' (F.Archetype (_, _, _, g, _)) out = do
--     (ListItemAction s a) <- ask
--     let go = view G._WRMT' (g out' s) a
--         out' = contramap (ListItemAction s) out
--     D.singleton <$> MP.mcatMaybes (lift go)

-- listGadget'
--     :: forall m o s a c e dtls plns cmds.
--        ( Eq s
--        , UniqueMember (S.Seq s) dtls
--        , UniqueMember F.WidgetCommand cmds
--        , UniqueMember (C.ReactorCommand (ListAction o s)) cmds
--        , R.Dispose s
--        )
--     => F.Archetype m o s a c e
--     -> PC.Output (ListAction o s)
--     -> G.GadgetT (ListAction o s) (F.Design dtls plns) STM (D.DList (Which cmds))
-- listGadget' (F.Archetype (mkEnt, _, _, g, _)) out = do
--     a <- ask
--     case a of
--         DestroyItemListAction s -> do
--             xs <- use (F.details . item)
--             let i = S.findIndexL (s ==) xs
--             i' <- A.afromMaybe i
--             let deleteAt j ys = let (x, y) = S.splitAt j ys
--                                 in case S.viewl y of
--                                    S.EmptyL -> x
--                                    _ S.:< y' -> x S.>< y'
--             (F.details . item @(S.Seq s)) %= deleteAt i'
--             (F.widgetPlan . F.deferredDisposables) %= (>> R.dispose s)
--             fmap pick <$> G.gadgetWith F.RenderAction F.widgetGadget
--         MakeItemListAction o -> do
--             pure $ D.singleton $ C.ReactorCommand out mkEnt


-- listGadgetry
--     :: forall m o s a c e dtls plns acts cmds case'.
--        ( Eq s
--        , Reinterpret '[ListItemAction s a, ListAction s] acts
--        , UniqueMember (S.Seq s) dtls
--        , UniqueMember (ListItemAction s a) acts
--        , UniqueMember (ListAction s) acts
--        , UniqueMember c cmds
--        , UniqueMember F.WidgetCommand cmds
--        , R.Dispose s
--        )
--     => F.Archetype m o s a c e
--     -> F.Gadgetry dtls plns '[ListItemAction s a, ListAction s] acts '[c, F.WidgetCommand] cmds




-- listPrototype
--     :: (UniqueMember InputAction acts, UniqueMember C.PropertyCommand cmds)
--     => F.Prototype '[] ols '[] dtls '[] plns '[] trigs '[InputAction] acts '[C.PropertyCommand] cmds
-- listPrototype =
--     (F.statically $ F.display d) `F.orPrototype`
--     (F.dynamically (F.gadgetry gadget))
--   where
--     d ls ps = lift $ R.lf "input" ls ps


-- data Schema k w (p :: R.Part) = Schema
--     { _className :: J.JSString
--     , _idx :: k
--     , _items :: M.Map k (R.Widget's p w)
--     , _itemsFilter :: R.OutlineOf w -> Bool
--     }

-- type Detail k w = Schema k w 'R.BaseObject'
-- type Outline k w = Schema k w 'R.Outline'

-- outline
--     :: ( R.ModelOf w ~ R.BaseModelOf w
--        , R.HasDetail w
--        , R.ToOutline w
--        )
--     => w -> Detail k w -> Outline k w
-- outline w (Schema a b c d) =
--         Schema a b (view (R.ival . R.detail w . to (R.outline w)) <$> c) d

-- mkDetail
--     :: (R.IsWidget w, R.ModelOf w ~ R.BaseModelOf w)
--     => w
--     -> Outline k w
--     -> F (R.Reactor (Action k w)) (Detail k w)
-- mkDetail w (Schema a b c d) = Schema
--     <$> pure a
--     <*> pure b
--     <*> M.traverseWithKey (\k i -> R.hoistWithAction (ListAction . ItemAction k) (R.mkBaseObject' w i)) c
--     <*> pure d

-- data Plan = Plan
--     { _componentPlan :: D.Component.Plan
--     , _renderPlan :: D.Render.Plan
--     , _disposePlan :: D.Dispose.Plan
--     } deriving (G.Generic)

-- makeClassyPrisms ''Action
-- makeClassyPrisms ''Action'
-- makeClassy ''Schema
-- makeClassy ''Plan

-- mkComponentPlan
--     :: D.Component.Display (Action k w) mdl
--     -> D.Render.Device mdl
--     -> D.Dispose.Device mdl
--     -> MVar mdl
--     -> F (R.Reactor (Action k w)) Plan
-- mkComponentPlan component' render' dispose' frm = Plan
--     <$> (R.mkComponentPlan component' frm)
--     <*> (R.hoistWithAction RenderAction $ R.mkPlan render')
--     <*> (R.hoistWithAction DisposeAction $ R.mkPlan dispose')

-- instance CD.Disposing Plan

-- instance (R.ModelOf w ~ R.BaseModelOf w, CD.Disposing (R.ObjectOf w)) =>
--          CD.Disposing (Detail k w) where
--     disposing s = CD.DisposeList $ foldr ((:) . CD.disposing) [] (s ^. items)

-- instance D.Component.HasPlan Plan where
--     plan = componentPlan

-- instance D.Render.HasPlan Plan where
--     plan = renderPlan

-- instance D.Dispose.HasPlan Plan where
--     plan = disposePlan

-- -- | Internal rendering used by the React render callback
-- render
--     :: (R.ModelOf w ~ R.BaseModelOf w, R.IsWidget w)
--     => R.ReactMlT Identity ()
--     -> w
--     -> Lens' mdl (Detail k w)
--     -> Lens' mdl Plan
--     -> (mdl -> R.RenderAttributes)
--     -> G.WindowT mdl R.ReactMl ()
-- render separator w dtl pln ra = do
--     s <- ask
--     let R.RenderAttributes (props, hdls) = ra s
--     xs <-
--         filter ((s ^. dtl . itemsFilter) . R.toOutline' w) .
--         fmap (view R.ival . snd) . -- get the item BaseModel
--         M.toList <$>
--         view (dtl . items) -- get the items
--     lift $
--         R.bh
--             "ul"
--             ([ ("key", s ^. pln . D.Component.key . to JE.toJS')
--             , ("className", s ^. dtl . className . to JE.toJS')
--             ] ++ props)
--             hdls $ do
--         let itemsWindows = view G._WindowT itemWindow <$> xs
--             separatedWindows = DL.intersperse separator itemsWindows
--         sequenceA_ separatedWindows
--   where
--     itemWindow = R.window w

-- gadget
--     :: (Ord k, R.ModelOf w ~ R.BaseModelOf w, R.IsWidget w)
--     => D.Render.Device mdl
--     -> D.Dispose.Device mdl
--     -> w
--     -> Lens' mdl (Detail k w)
--     -> Lens' mdl Plan
--     -> G.Gadget (Action k w) (R.Shared mdl) (D.DList (Command k w))
-- gadget render' dispose' w dtl pln =
--         (fmap RenderCommand <$> magnify _RenderAction (R.gadget render'))
--     <|> (fmap DisposeCommand <$> magnify _DisposeAction (R.gadget dispose'))
--     <|> (magnify _ListAction (listGadget render' w dtl pln))

-- listGadget
--     :: (Ord k, R.ModelOf w ~ R.BaseModelOf w, R.IsWidget w)
--     => D.Render.Device mdl
--     -> w
--     -> Lens' mdl (Detail k w)
--     -> Lens' mdl Plan
--     -> G.Gadget (Action' k w) (R.Shared mdl) (D.DList (Command k w))
-- listGadget render' w dtl pln = do
--     a <- ask
--     case a of
--         DestroyItemAction k -> do
--             -- queue up callbacks to be released after rerendering
--             ret <- runMaybeT $ do
--                 itemObject <- MaybeT $ use (R.ival . dtl . items . at k)
--                 (R.ival . pln . D.Dispose.deferredDisposables) %= (`D.snoc` CD.disposing itemObject)
--                 -- Remove the todo from the model
--                 (R.ival . dtl . items) %= M.delete k
--                 -- on re-render the todo Shim will not get rendered and will be removed by react
--                 lift rerender
--             maybe (pure mempty) pure ret

--         MakeItemAction keyReactor mkItemOutline -> do
--             n <- keyReactor <$> use (R.ival . dtl . idx)
--             (R.ival . dtl . idx) .= n
--             pure $ D.singleton $ ReactorCommand $ C.Reactor.Reactor Command $ do
--                 sm <- R.hoistWithAction (ListAction . ItemAction n) (
--                     mkItemOutline n >>= R.mkBaseObject' w)
--                 pure . ListAction $ AddItemAction n sm

--         AddItemAction n v -> do
--             (R.ival . dtl . items) %= M.insert n v
--             rerender

--         ItemAction k _ -> fmap (ListCommand . ItemCommand k) <$>
--             (magnify (_ItemAction . to snd)
--             (zoom (R.ival . dtl . items . at k . _Just) (R.gadget w)))

--         SetFilterAction ftr -> do
--             (R.ival . dtl . itemsFilter) .= ftr
--             rerender
--   where
--       rerender = fmap RenderCommand <$> G.withGadgetT D.Render.RenderAction (R.gadget render')

-- type Widget k w mdl = R.Widget (Action k w) (Outline k w) (Detail k w) Plan (Command k w) mdl

-- widget
--     :: (Ord k, R.ModelOf w ~ R.BaseModelOf w, R.IsWidget w)
--     => R.ReactMlT Identity ()
--     -> w
--     -> Lens' mdl (Detail k w)
--     -> Lens' mdl Plan
--     -> (mdl -> R.RenderAttributes)
--     -> Widget k w mdl
-- widget separator w dtl pln ra =
--     R.Widget
--         dtl
--         pln
--         (outline w)
--         (mkDetail w)
--         (mkComponentPlan component' render' dispose')
--         (R.window component')
--         (gadget render' dispose' w dtl pln)
--   where
--     component' =
--         D.Component.display
--             (pln . componentPlan)
--             (render separator w dtl pln ra)
--             (R.componentAttributes render' <> R.componentAttributes dispose')
--     render' = D.Render.device (pln . renderPlan)
--     dispose' = D.Dispose.device (pln . disposePlan)
