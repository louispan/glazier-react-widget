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
newtype Archetype m r s h a c cmds =
    Archetype
    ( s -> R.ReactMlT m () -- display
    , s -> m r -- builder frmSpec
    , r -> m s -- builder mkInactiveEntity
    , F.Handler' m s h c -- event handlers
    -- activator needs externally provided handlers
    , F.Activator' m s a cmds
    )

-- | Create a Prototype from an Archetype.
-- This wraps the specifications and requirements in an additional layer of 'Many'.
-- Therefore, this is NOT the opposite of 'implement', that is:
--
-- @
-- implant . implement /= id
-- @
implant
    :: forall m r reqs s specs h hs ts a as c cs cmds.
       ( R.MonadReactor m
       , UniqueMember r reqs
       , UniqueMember (IORef s) specs
       , Diversify a as
       , Diversify c cs
       , Reinterpret' h hs
       )
    => Archetype m r s h a c cmds
    -> F.Prototype m '[r] reqs '[IORef s] specs h hs '[] ts a as c cs cmds
implant (Archetype (rnd, frmEnt, mkEnt, hdlEnt, activateEnt)) = F.Prototype
    ( F.divIfNeeded disp
    , F.Builder (fromSpec, mkSpec)
    , F.handler (F.implantHandler' fromThis hdlEnt)
    , F.boring
    , F.Activator activateDesign
    )
  where
    toActivatorHdl
      :: F.Handler' m (F.Design specs) as cmds
         -> IORef (F.Design specs)
         -> F.Handler' m s a cmds
    toActivatorHdl hdl' this _ a = hdl' this (diversify @a @as a)
    fromThis :: R.MonadReactor m => IORef (F.Design specs) -> m (IORef s)
    fromThis this = do
        obj <- R.doReadIORef this
        pure (obj ^. (F.specifications . item))
    activateDesign exec hdl' this = do
        s <- lift . lift $ fromThis this
        activateEnt exec (toActivatorHdl hdl' this) s
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
       , Diversify c cmds
       , acts ~ Complement (AppendUnique a t) h
       , r' ~ ([JE.Property] ': r)
       )
    => F.Prototype m r r s s h h t t a a c c cmds
    -> Archetype m (Many r') (F.Design s) h acts c cmds
implement (F.Prototype (disp, bldr, hdl, ts, activtr)) = Archetype
  ( F.componentWindow
  , fromEntity bldr
  , mkInactiveEntity bldr
  , F.runHandler hdl
  , activateEntity activtr disp ts hdl
  )

activateEntity
    :: forall m s h t a acts c cmds.
       ( R.MonadReactor m
       , acts ~ Complement (AppendUnique a t) h
       , NFData (Which t)
       , Reinterpret' acts a
       , Reinterpret' acts t
       , Reinterpret' h a
       , Reinterpret' h t
       , Diversify c cmds
       )
    => F.Activator m (F.Design s) a a cmds
    -> F.Display m s
    -> F.Triggers t t
    -> F.Handler m (F.Design s) h h c c -- handler for some of the triggers or builder
    -> F.Executor' cmds
    -> F.Handler' m (F.Design s) acts cmds -- externally provided handler for builder and remaining triggers
    -> IORef (F.Design s)
    -> MaybeT (StateT (R.Disposable ()) m) (m ())
activateEntity (F.Activator activateDesign) disp ts (F.Handler internalHdl) exec externalHdl this = do
    obj <- hoist lift $ F.initDesign w this
    ls <- lift . lift $ F.mkListeners exec (internalHdls this) (M.toList $ F.runTriggers ts)
    let obj' = obj & F.plan . F.listeners %~ (<> ls)
    x <- activateDesign exec activatorHdls this
        -- only write if activation succeeds
    pure (x >> R.doWriteIORef this obj')
  where
    -- any actions required by @a@, but not handled by @h@, must be handled by externally provided handler
    -- any actions required by @a@ and handled by @h@
    activatorHdls this' a =
        (fmap diversify <$> F.reinterpretHandler' @h internalHdl this' a) <|>
        F.reinterpretHandler' @acts externalHdl this' a
    -- any actions required by @t@, but not handled by @h@, must be handled by externally provided handler
    -- any actions required by @t@ and handled by @h@
    internalHdls this' a =
        (fmap diversify <$> F.reinterpretHandler' @h internalHdl this' a) <|>
        F.reinterpretHandler' @acts externalHdl this' a
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
fromEntity (F.Builder (fromSpec, _)) obj = do
    let ps = obj ^. F.properties
        ss = obj ^. F.specifications
    rs <- fromSpec ss
    pure (ps ./ rs)
