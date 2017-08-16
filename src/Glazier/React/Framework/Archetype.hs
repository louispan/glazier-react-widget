{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Glazier.React.Framework.Archetype where

-- import Control.Applicative
import Control.Concurrent.STM
import Control.Lens
import Control.Monad.Free.Church
import Control.Monad.Trans.Class
-- import Control.Monad.Trans.Maybe
import Data.Diverse.Lens
import qualified Data.DList as D
import Data.Semigroup
-- import Data.Foldable
-- import Data.Proxy
-- import qualified Data.Map.Strict as M
-- import qualified Glazier as G
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Builder as F
import qualified Glazier.React.Framework.Display as F
-- import qualified Glazier.React.Framework.Executor as F
-- import qualified Glazier.React.Framework.Gadgetry as F
import qualified Glazier.React.Framework.Prototype as F
-- import qualified Glazier.React.Framework.Trigger as F
-- import qualified Glazier.React.Framework.TypeLevel as F
import qualified Glazier.React.Framework.Widget as F
import qualified JavaScript.Extras as JE
import qualified Pipes.Concurrent as PC

-- | NB. a must contain [JE.Property]
newtype Archetype r s = Archetype ( r -> F R.Reactor s
                                  , s -> STM r
                                  , s -> R.ReactMlT STM ())

-- | Finalize the design of a 'Prototype' and convert the make functions into making an Entity.
-- This also adds [JE.Property] to @s@
commission
    :: (UniqueMember [JE.Property] r', r' ~ ([JE.Property] ': r))
    => PC.Output (R.Disposable ()) -> F.Prototype r r s s -> Archetype (Many r') (TMVar (F.Design s))
commission dc (F.Prototype (F.Builder (mkSpec, fromSpec), disp, ts)) = Archetype (mkEntity, fromEntity, rnd)
  where
    mkEntity rs = let (ps, xs) = viewf rs in do
        ss <- mkSpec xs
        d <- R.doSTM newEmptyTMVar
        d' <- F.mkDesign dc w (D.toList ts) ps ss d
        R.doSTM (putTMVar d d')
        pure d
    w = F.renderDisplay (F.widgetDisplay <> disp)
    fromEntity d = do
        d' <- takeTMVar d
        let ps = d' ^. F.properties
            ss = d' ^. F.specifications
        rs <- fromSpec ss
        pure (ps ./ rs)
    rnd d = lift (takeTMVar d) >>= F.componentWindow

-- | Create a Prototype from an Archetype.
-- NB. This is NOT the opposite of 'comission', that is:
--
-- @
-- redraft . complete /= id
-- @
redraft
    :: ( UniqueMember r reqs
       , UniqueMember s specs
       )
    => Archetype r s
    -> F.Prototype '[r] reqs '[s] specs
redraft (Archetype (mkEntity, fromEntity, rnd)) = F.Prototype
    ( F.Builder (mkSpec, fromSpec)
    , F.divIfNeeded disp
    , mempty)
  where
    mkSpec rs = let r = fetch rs in single <$> mkEntity r
    fromSpec ss = let s = fetch ss in single <$> fromEntity s
    disp d = let s = d ^. (F.specifications . item) in rnd s

linkSpecification :: Iso' s' s -> Archetype r s -> Archetype r s'
linkSpecification l (Archetype (mkEntity, fromEntity, disp)) =
        Archetype ( fmap (review l) . mkEntity
                  , fromEntity . view l
                  , magnify l disp)

linkRequirement :: Iso' r' r -> Archetype r s -> Archetype r' s
linkRequirement l (Archetype (mkEntity, fromEntity, disp)) =
        Archetype ( mkEntity . view l
                  , fmap (review l) . fromEntity
                  , disp)
