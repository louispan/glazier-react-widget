{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
import Data.Semigroup (Semigroup(..))

type Handler' v s a c = (TMVar v -> ReifiedLens' v s -> Which a -> MaybeT STM (Which c))

-- instance Functor (Handler' v s a) where
--     fmap f (Handler' hdl) = Handler' (\v l -> fmap f . hdl v l)

-- instance Applicative (Handler' v s a) where
--     pure a = Handler' (\_ _ _ -> pure a)
--     (Handler' hdl1) <*> (Handler' hdl2) = Handler' (\v l a -> hdl1 v l a <*> hdl2 v l a)

-- instance Alternative (Handler' v s a) where
--     empty = Handler' (\_ _ _ -> empty)
--     (Handler' hdl1) <|> (Handler' hdl2) = Handler' (\v l a -> hdl1 v l a <|> hdl2 v l a)

-- instance Profunctor (Handler' v s) where
--     dimap f g (Handler' hdl) = Handler' (\v l -> fmap g . hdl v l . f)

newtype Handler v s (a :: [Type]) acts (c :: [Type]) cmds = Handler
    ( Proxy a
    , Proxy c
    , Handler' v s acts cmds
    )

instance Semigroup (Handler v s '[] acts '[] cmds) where
    _ <> _ = Handler (Proxy, Proxy,  \_ _ _ -> empty)

instance Monoid (Handler v s '[] acts '[] cmds) where
    mempty = Handler (Proxy, Proxy,  \_ _ _ -> empty)
    mappend = (<>)

getHandler :: Handler v s a acts c cmds -> Handler' v s acts cmds
getHandler (Handler (_, _, hdl)) = hdl

handler
    :: (UniqueMember a acts, UniqueMember c cmds)
    => (TMVar v -> Lens' v s -> a -> MaybeT STM c) -> Handler v s '[a] acts '[c] cmds
handler f = Handler (Proxy, Proxy, \v (Lens l) a -> do
                            a' <- MaybeT . pure $ trial' a
                            pick <$> f v l a')

-- | mempty is also Identity for 'orHandler'
-- NB. Due to the use of <|> only the first handler for a particular action will be used.
-- This is to prevent running handlers twice for the one action.
-- This will be compile time check with @Append a1 a2@ and @UniqueMember@ constraints.
orHandler
    :: Handler v s a1 acts c1 cmds
    -> Handler v s a2 acts c2 cmds
    -> Handler v s (Append a1 a2) acts (AppendUnique c1 c2) cmds
orHandler (Handler (_, _, f)) (Handler (_, _, g)) =
    Handler (Proxy, Proxy, \v l -> liftA2 (<|>) (f v l) (g v l))

-- | For example
--
-- @
-- handleSpecifications :: Handler v (Many specs) h acts -> Handler v (F.Design specs) h acts
-- handleSpecifications = handleWith F.specifications
-- @
magnifyHandler :: Lens' t s -> Handler v s a acts c cmds -> Handler v t a acts c cmds
magnifyHandler l (Handler (pa, pc, hdl)) = Handler (pa, pc, magnifyHandler' l hdl)

magnifyHandler' :: Lens' t s -> Handler' v s a c -> Handler' v t a c
magnifyHandler' l f v (Lens l') = f v (Lens (l' . l))