{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Prototypes.Input where

import Control.Applicative
import Control.DeepSeq
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Maybe
import qualified Control.Monad.Trans.Maybe.Extras as Ex
import Data.Diverse.Lens
import qualified Data.DList as DL
import qualified Data.JSString as J
import qualified GHC.Generics as G
import qualified Glazier.React as R
import qualified Glazier.React.Actions as A
import qualified Glazier.React.Framework as F
import qualified Glazier.React.Prototypes.WithRef as W
import qualified JavaScript.Extras as JE
import qualified Parameterized.Data.Monoid as P

input ::
    ( HasItem' R.EventTarget s
    , HasItem' [F.Trait] s
    , HasItem' [JE.Property] s
    , HasItem' [JE.Property] i
    , HasItem' [R.Listener] s
    , R.MonadReactor x m
    )
    => [F.Trait]
    -> F.Prototype m v i s
        (Many '[ [JE.Property] ])
        (Many '[ [R.Listener], [F.Trait], [JE.Property], R.EventTarget])
        x
        (Which '[]) (Which '[])
        (Which '[]) (Which '[])
input ts = F.widget "input" ts W.withRef

-- data SubmitInput = SubmitInput R.EventTarget J.JSString
--     deriving (G.Generic, NFData)
-- newtype CancelInput = CancelInput R.EventTarget
--     deriving (G.Generic, NFData)

-- input
--     :: ( R.MonadReactor x m
--        , HasItem' (DL.DList JE.Property) p
--        , HasItem' (DL.DList JE.Property) s
--        , HasItem' (DL.DList R.Listener) s
--        )
--     => F.Prototype m v p s
--             (Many '[DL.DList JE.Property])
--             (Many '[DL.DList JE.Property, DL.DList R.Listener])
--             x
--             (Which '[SubmitInput, CancelInput])
--             (Which '[])
--             (Which '[])
-- input =
--     F.Prototype
--         ( F.Display $ \(_, ss) -> R.lf "input"
--             (DL.toList $ ss ^. item' @(DL.DList R.Listener))
--             (DL.toList $ ss ^. item' @(DL.DList JE.Property))
--         , F.buildItem @(DL.DList JE.Property)
--             `P.pmappend` F.hardcodeItem @(DL.DList R.Listener) DL.empty
--         , F.triggerExecutor' [F.Trigger ("onKeyDown", Ex.fromMaybeT . (A.fireKeyDownKey >=> go))]
--         )
--   where
--     go (A.KeyDownKey target k) =
--         case k of
--             "Escape" -> pure . pick $ CancelInput target
--             "Enter" -> do
--                 v <- MaybeT $ JE.fromJS' <$> JE.getProperty "value" (JE.toJS target)
--                 pure . pick $ SubmitInput target v
--             _ -> empty
