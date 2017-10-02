{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.React.Framework.Handler where

import Control.Applicative
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State.Strict
import Control.Concurrent.STM
import Data.Diverse
import qualified Data.DList as DL
import Data.Kind
import Data.Proxy
import qualified Glazier.React.Framework.Widget as F

-- NB. Reififed helps type inference because
-- the output doesn't depends on v or s
type Handler' m s a c = IORef s -> Which a -> MaybeT m (DL.DList (Which c))

newtype Handler s (a :: [Type]) acts (c :: [Type]) cmds = Handler
    ( Proxy a
    , Proxy c
    , Handler' s acts cmds
    )

-- | identity for 'orHandler'
ignore :: Handler s '[] acts '[] cmds
ignore = Handler (Proxy, Proxy, \_ _ -> empty)

runHandler :: Handler s a acts c cmds -> Handler' s acts cmds
runHandler (Handler (_, _, hdl)) = hdl

tvarHandler
    :: (UniqueMember a acts, UniqueMember c cmds)
    => (TVar s -> a -> MaybeT (StateT s STM) c) -> Handler s '[a] acts '[c] cmds
tvarHandler f = handler f'
    where
      f' v' a = F.usingTVar v' (f v' a)

handler
    :: (UniqueMember a acts, UniqueMember c cmds)
    => (TVar s -> a -> MaybeT STM c) -> Handler s '[a] acts '[c] cmds
handler f = Handler (Proxy, Proxy, \v a -> do
                            a' <- MaybeT . pure $ trial' a
                            (DL.singleton . pick) <$> f v a')

-- | Due to @Append a1 a2@ and @UniqueMember@ constraints, it is a compile time
-- error to `orHandlers` of the same type.
-- NB. The use of <|> only the first handler for a particular action will be used.
orHandler
    :: Handler s a1 acts c1 cmds
    -> Handler s a2 acts c2 cmds
    -> Handler s (Append a1 a2) acts (AppendUnique c1 c2) cmds
orHandler (Handler (_, _, f)) (Handler (_, _, g)) =
    Handler (Proxy, Proxy, \v -> liftA2 (<|>) (f v) (g v))

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
