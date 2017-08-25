{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Framework.Handler where

import Control.Applicative
import Control.Monad.Trans.Maybe
import Control.Concurrent.STM
import Control.Lens
import Data.Diverse
import Data.Kind
import Data.Proxy

newtype Handler v s (h :: [Type]) acts = Handler
    ( Proxy h
    , TMVar v -> ReifiedLens' v s -> Which acts -> MaybeT STM ()
    )

noop :: Handler v s '[] acts
noop = Handler (Proxy, \_ _ _ -> pure ())

handler :: UniqueMember h acts => (TMVar v -> Lens' v s -> h -> MaybeT STM ()) -> Handler v s '[h] acts
handler f = Handler (Proxy, \v (Lens l) -> (f v l =<<) . MaybeT . pure . trial')

-- | NB. it is expected that there is only oever one handler for each action.
-- This is to prevent running handlers twice for the one action.
-- This will be checked by the compiler with @Append a1 a2@ and @UniqueMember@ constraints.
andHandler :: Handler v s h1 acts -> Handler v s h2 acts -> Handler v s (Append h1 h2) acts
andHandler (Handler (Proxy, f)) (Handler (Proxy, g)) =
    Handler (Proxy, \v l -> liftA2 (<|>) (f v l) (g v l))

-- | For example
--
-- @
-- handleSpecifications :: Handler v (Many specs) h acts -> Handler v (F.Design specs) h acts
-- handleSpecifications = handleWith F.specifications
-- @
handleUnder :: Lens' t s -> Handler v s h acts -> Handler v t h acts
handleUnder l (Handler (p, f)) = Handler (p, \v (Lens l') -> f v (Lens (l' . l)))

-- dispatch :: Proxy h' -> Prism' (Which acts') (Which acts) -> Handler v s h acts' -> Handler v s h' acts
-- dispatch p l (Handler (_, f)) = Handler (p, \v l' a -> f v l' (review l a))

-- dispatch :: Prism' (Which acts') (Which acts) -> Handler v s h acts -> Handler v s h acts'
-- dispatch l (Handler (p, f)) = Handler (p, \v l' a -> case preview l a of
--                                                          Nothing -> empty
--                                                          Just a' -> f v l' a')
