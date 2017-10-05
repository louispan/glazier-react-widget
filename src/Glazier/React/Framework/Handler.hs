{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Framework.Handler where

import Control.Applicative
import Control.Monad.Trans.Maybe
import Data.Diverse
import qualified Data.DList as DL
import Data.IORef
import Data.Kind

-- NB. Reififed helps type inference because
-- the output doesn't depends on v or s
type Handler' m s a c = IORef s -> Which a -> MaybeT m (DL.DList (Which c))

newtype Handler m s (a :: [Type]) acts (c :: [Type]) cmds = Handler (Handler' m s acts cmds)

-- | identity for 'orHandler'
ignore :: Monad m => Handler m s '[] acts '[] cmds
ignore = Handler (\_ _ -> empty)

runHandler :: Handler m s a acts c cmds -> Handler' m s acts cmds
runHandler (Handler hdl) = hdl

-- ioRefHandler
--     :: (MonadReactor m => UniqueMember a acts, UniqueMember c cmds)
--     => (IORef s -> a -> MaybeT (StateT s m) c) -> Handler m s '[a] acts '[c] cmds
-- ioRefHandler f = handler f'
--     where
--       f' v' a = F.usingIORef v' (f v' a)

handler
    :: (Monad m, UniqueMember a acts, UniqueMember c cmds)
    => (IORef s -> a -> MaybeT m c) -> Handler m s '[a] acts '[c] cmds
handler f = Handler (\v a -> do
    a' <- MaybeT . pure $ trial' a
    (DL.singleton . pick) <$> f v a')

-- | Due to @Append a1 a2@ and @UniqueMember@ constraints, it is a compile time
-- error to `orHandlers` of the same type.
-- NB. The use of <|> only the first handler for a particular action will be used.
orHandler
    :: Monad m
    => Handler m s a1 acts c1 cmds
    -> Handler m s a2 acts c2 cmds
    -> Handler m s (Append a1 a2) acts (AppendUnique c1 c2) cmds
orHandler (Handler f) (Handler g) =
    Handler (\v -> liftA2 (<|>) (f v) (g v))

-- -- | For example
-- --
-- -- @
-- -- handleSpecifications :: Handler v (Many specs) h acts -> Handler v (F.Design specs) h acts
-- -- handleSpecifications = magnifyHandler F.specifications
-- -- @
-- -- magnifyHandler :: Lens' t s -> Handler s a acts c cmds -> Handler t a acts c cmds
-- magnifyHandler :: Lens' t s -> Handler v s a acts c cmds -> Handler v t a acts c cmds
-- magnifyHandler l (Handler (pa, pc, hdl)) = Handler (pa, pc, magnifyHandler' l hdl)

-- magnifyHandler' :: Lens' t s -> Handler' v s a c -> Handler' v t a c
-- magnifyHandler' l f v (Lens l') = f v (Lens (l' . l))
