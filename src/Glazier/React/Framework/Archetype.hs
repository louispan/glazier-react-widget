{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Glazier.React.Framework.Archetype where

import Control.Applicative
import Control.DeepSeq
import Control.Lens
import Control.Monad.Morph
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State.Strict
import Data.Diverse.Lens
import qualified Data.Map.Strict as M
import Data.IORef
import Data.Semigroup
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Activator as F
import qualified Glazier.React.Framework.Builder as F
import qualified Glazier.React.Framework.Display as F
import qualified Glazier.React.Framework.Executor as F
import qualified Glazier.React.Framework.Handler as F
import qualified Glazier.React.Framework.Prototype as F
import qualified Glazier.React.Framework.Trigger as F
import qualified Glazier.React.Framework.Widget as F
import qualified JavaScript.Extras as JE

-- | An archetype is an 'implement'ation of a 'Prototype' using a 'R.ReactComponent'.
-- It contains almost all the information of a 'Prototype', without the 'Trigger's, nor the ability to compose
-- multiple archetypes together.
-- 'Archetype' can be converted into a 'Prototype' to be composed with other 'Prototypes'.
newtype Archetype m r s h a hc acs =
    Archetype
    ( s -> R.ReactMlT m () -- display
    , s -> m r -- builder frmSpec
    , r -> m s -- builder mkInactiveEntity
    , F.Handler' m s h hc -- event handlers
    -- activator needs externally provided handlers
    , F.Activator' m s a acs
    )

-- | Create a Prototype from an Archetype.
-- This wraps the specifications and requirements in an additional layer of 'Many'.
-- Therefore, this is NOT the opposite of 'implement', that is:
--
-- @
-- implant . implement /= id
-- @
implant
    :: forall m r reqs s specs h hs ts a as hc hcs acs.
       ( R.MonadReactor m
       , UniqueMember r reqs
       , UniqueMember (IORef s) specs
       , Diversify a as
       , Diversify hc hcs
       , Reinterpret' h hs
       )
    => Archetype m r s h a hc acs
    -> F.Prototype m '[r] reqs '[IORef s] specs h hs '[] ts a as hc hcs acs
implant (Archetype (rnd, frmEnt, mkEnt, hdlEnt, activateEnt)) = F.Prototype
    ( F.divIfNeeded disp
    , F.Builder (fromSpec, mkSpec)
    , F.handler (F.implantHandler' fromThis hdlEnt)
    , F.boring
    , F.Activator activateDesign
    )
  where
    toActivatorHdl
      :: F.Handler' m (F.Design specs) as acs
         -> IORef (F.Design specs)
         -> F.Handler' m s a acs
    toActivatorHdl hdl' v _ a = hdl' v (diversify @a @as a)
    fromThis :: R.MonadReactor m => IORef (F.Design specs) -> m (IORef s)
    fromThis v = do
        d <- R.doReadIORef v
        pure (d ^. (F.specifications . item))
    activateDesign exec hdl' v = do
        s <- lift . lift $ fromThis v
        activateEnt exec (toActivatorHdl hdl' v) s
    mkSpec rs = do
        let r = fetch rs
        e <- mkEnt r
        single <$> R.doNewIORef e
    fromSpec ss = do
        let s = fetch ss
        single <$> (R.doReadIORef s >>= frmEnt)
    disp d = let s = d ^. (F.specifications . item)
             in lift (R.doReadIORef s) >>= rnd

-- | Finalize the design of a 'Prototype' and convert the make functions into making an Entity.
-- This also adds [JE.Property] to @s@
implement
    :: ( R.MonadReactor m
       , NFData (Which t)
       , Reinterpret' acts a
       , Reinterpret' acts t
       , Reinterpret' h a
       , Reinterpret' h t
       , Diversify hc acs
       , acts ~ Complement (AppendUnique a t) h
       , r' ~ ([JE.Property] ': r)
       )
    => F.Prototype m r r s s h h t t a a hc hc acs
    -> Archetype m (Many r') (F.Design s) h acts hc acs
implement (F.Prototype (disp, bldr, hdl, ts, activtr)) = Archetype
  ( F.componentWindow
  , fromEntity bldr
  , mkInactiveEntity bldr
  , F.runHandler hdl
  , activateEntity activtr disp ts hdl
  )

activateEntity
    :: forall m s h t a acts hc acs.
       ( R.MonadReactor m
       , acts ~ Complement (AppendUnique a t) h
       , NFData (Which t)
       , Reinterpret' acts a
       , Reinterpret' acts t
       , Reinterpret' h a
       , Reinterpret' h t
       , Diversify hc acs
       )
    => F.Activator m (F.Design s) a a acs
    -> F.Display m s
    -> F.Triggers t t
    -> F.Handler m (F.Design s) h h hc hc -- handler for some of the triggers or builder
    -> F.Executor' acs
    -> F.Handler' m (F.Design s) acts acs -- externally provided handler for builder and remaining triggers
    -> IORef (F.Design s)
    -> MaybeT (StateT (R.Disposable ()) m) (m ())
activateEntity (F.Activator activateDesign) disp ts (F.Handler internalHdl) exec externalHdl v = do
    d <- hoist lift $ F.initDesign w v
    ls <- lift . lift $ F.mkListeners exec (internalHdls v) (M.toList $ F.runTriggers ts)
    let d' = d & F.plan . F.listeners %~ (<> ls)
    x <- activateDesign exec activatorHdls v
        -- only write if activation succeeds
    pure (x >> R.doWriteIORef v d')
  where
    -- any actions required by @a@, but not handled by @h@, must be handled by externally provided handler
    -- any actions required by @a@ and handled by @h@
    activatorHdls v' a =
        (fmap diversify <$> F.reinterpretHandler' @h internalHdl v' a) <|>
        F.reinterpretHandler' @acts externalHdl v' a
    -- any actions required by @t@, but not handled by @h@, must be handled by externally provided handler
    -- any actions required by @t@ and handled by @h@
    internalHdls v' a =
        (fmap diversify <$> F.reinterpretHandler' @h internalHdl v' a) <|>
        F.reinterpretHandler' @acts externalHdl v' a
    w = F.renderDisplay (F.widgetDisplay <> disp)

mkInactiveEntity
    :: forall m r r' s.
       ( Monad m
       , r' ~ ([JE.Property] ': r))
    => F.Builder m r r s s
    -> Many r'
    -> m (F.Design s)
mkInactiveEntity (F.Builder (_, mkSpec)) rs = do
    let (ps, xs) = viewf rs
    ss <- mkSpec xs -- externalToBuilderHdl xs
    pure $ F.inactiveDesign ps ss

fromEntity :: (Monad m, r' ~ ([JE.Property] ': r))
    => F.Builder m r r s s -> F.Design s -> m (Many r')
fromEntity (F.Builder (fromSpec, _)) d = do
    let ps = d ^. F.properties
        ss = d ^. F.specifications
    rs <- fromSpec ss
    pure (ps ./ rs)

-- mkBasicEntity :: (r -> F R.Reactor s) -> r -> F R.Reactor (TMVar s)
-- mkBasicEntity mkEnt r = do
--     v <- R.doSTM newEmptyTMVar
--     s <- mkEnt v (Lens id) r
--     R.doSTM $ putTMVar v s
--     pure v

-- toDelegate
--     :: F.Handler' s a c
--     -> F.Executor' c
--     -> TMVar v
--     -> ReifiedLens' v s -- reified because output doesn't have lens type variables
--     -> Which a
--     -> MaybeT IO ()
-- toDelegate hdl exec v l a = hoist atomically (hdl v l a) >>= exec

-- tweakAction :: Prism' (Which a') (Which a) -> Archetyper v r s a c -> Archetyper v r s a' c
-- tweakAction l (Archetyper (mkEnt, frmEnt, dszpEnt)) =
--     Archetyper (\hdl -> mkEnt (\v l' -> hdl v l' . review l), frmEnt, dspEnt)

-- tweakCommand :: Iso' (Which c') (Which c) -> Archetyper v r s a c -> Archetyper v r s a c'
-- tweakCommand l (Archetyper (mkEnt, frmEnt, dspEnt)) =
--     Archetyper (\hdl exec -> mkEnt (\v l' -> fmap (view l) . hdl v l') (exec . review l), frmEnt, dspEnt)

-- tweakSpecification :: Iso' s' s -> Archetype v r s -> Archetype v r s'
-- tweakSpecification l (Archetype (mkEnt, frmEnt, disp)) =
--     Archetype ( \v (Lens l') r -> review l <$> mkEnt v (Lens (l' . l)) r
--               , frmEnt . view l
--               , magnify l disp)

-- tweakSpecification' :: Iso' s' s -> Archetyper v r s a c -> Archetyper v r s' a c
-- tweakSpecification' l f hdl exec = tweakSpecification l (f (F.magnifyHandler' (from l) hdl) exec)

-- tweakRequirement :: Iso' r' r -> Archetype v r s -> Archetype v r' s
-- tweakRequirement l (Archetype (mkEnt, frmEnt, disp)) =
--     Archetype ( \v l' r -> mkEnt v l' (view l r)
--               , fmap (review l) . frmEnt
--               , disp)

-- tweakRequirement' :: Iso' r' r -> Archetyper v r s a c -> Archetyper v r' s a c
-- tweakRequirement' l f hdl exec = tweakRequirement l (f hdl exec)
