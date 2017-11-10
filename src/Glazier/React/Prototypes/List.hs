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

module Glazier.React.Prototypes.List where

import Control.Lens
import qualified Data.DList as DL
import Data.Diverse.Lens
import Data.Foldable
-- import qualified Data.JSString as J
-- import Data.Maybe
import qualified Data.Sequence as S
import qualified Glazier.React as R
import qualified Glazier.React.Framework as F
-- import qualified Glazier.React.Commands as C
import qualified JavaScript.Extras as JE
-- import qualified Pipes.Concurrent as PC

-- | List specific actions
data DeleteListItem = DeleteListItem
data MakeListItem r = MakeListItem r

-- data ListItemAction s a = ListItemAction s a

    -- | MakeItemAction (F (R.Reactor a) o)
    -- | SetFilterAction (R.OutlineOf w -> Bool)
    -- | SetSortAction (R.OutlineOf w -> Bool)

listBuilder
    :: forall m p s a b c ps ss.
    ( Applicative m
    , UniqueMember (S.Seq p) ps
    , UniqueMember (S.Seq s) ss
    )
    => F.Archetype m p s a b c
    -> F.Builder m (Many ps) (Many ss) (Many '[S.Seq p]) (Many '[S.Seq s])
listBuilder (F.Archetype (F.Builder (F.MkPlan mkPln, F.MkModel mkMdl), _, _, _)) =
    F.Builder (F.MkPlan mkPln', F.MkModel mkMdl')
  where
    mkPln' ss = single <$> traverse mkPln (fetch @(S.Seq s) ss)
    mkMdl' ps = single <$> traverse mkMdl (fetch @(S.Seq p) ps)

listDisplay
    :: forall m p s a b c ss.
    ( R.MonadReactor m
    , UniqueMember (S.Seq s) ss
    , UniqueMember (DL.DList JE.Property) ss
    , UniqueMember (DL.DList R.Listener) ss
    )
    => F.Archetype m p s a b c
    -> F.Display m (Many ss) ()
listDisplay (F.Archetype (_, _, _, F.Display disp)) = F.Display $ \ss ->
    let xs = ss ^. (item @(S.Seq s))
        xs' = toLi <$> xs
        toLi s = R.bh "li" [] [] (disp s)
    in R.bh "ul"
        (DL.toList $ fetch @(DL.DList R.Listener) ss)
        (DL.toList $ fetch @(DL.DList JE.Property) ss)
        (mconcat $ toList xs')

broadcastHandler
    :: forall m p s a b c ss.
    ( R.MonadReactor m
    , UniqueMember (S.Seq s) ss
    , UniqueMember (DL.DList JE.Property) ss
    , UniqueMember (DL.DList R.Listener) ss
    )
    => F.Archetype m p s a b c
    -> F.Display m (Many ss) ()
broadcastHandler (F.Archetype (_, _, _, F.Display disp)) = F.Display $ \ss ->
    let xs = ss ^. (item @(S.Seq s))
        xs' = toLi <$> xs
        toLi s = R.bh "li" [] [] (disp s)
    in R.bh "ul"
        (DL.toList $ fetch @(DL.DList R.Listener) ss)
        (DL.toList $ fetch @(DL.DList JE.Property) ss)
        (mconcat $ toList xs')

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