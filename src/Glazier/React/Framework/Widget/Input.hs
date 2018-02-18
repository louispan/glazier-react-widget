{-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE DeriveAnyClass #-}
-- {-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Glazier.React.Framework.Widget.Input where

-- import Control.Applicative
-- import Control.DeepSeq
-- import Control.Lens
-- import Control.Monad
-- import Control.Monad.Trans.Maybe
-- import qualified Control.Monad.Trans.Maybe.Extras as Ex
import Data.Diverse.Lens
-- import qualified Data.DList as DL
-- import qualified Data.JSString as J
import Data.Tagged
-- import qualified GHC.Generics as G
import qualified Glazier.React as R
import qualified Glazier.React.Framework.Core as F
-- import qualified Glazier.React.Widget.Actions as A
import qualified Glazier.React.Framework.Widget.WithRef as W
import qualified JavaScript.Extras as JE
-- import qualified Parameterized.Data.Monoid as P

input :: forall t x m v i s.
    ( HasItemTag' t R.EventTarget s
    , HasItemTag' t [R.Listener] s
    , R.MonadReactor x m
    )
    => (F.Frame m s -> [JE.Property])
    -> F.Prototype m v i s
        (Many '[])
        (Many '[Tagged t [R.Listener], Tagged t R.EventTarget])
        (Which '[])
        (Which '[]) (Which '[])
input f = F.widget @t "input" f W.withRef

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
