{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Glazier.React.Effect.EventTarget where

import Glazier.React

type CmdEventTarget c = Cmd EffectEventTarget c

-- Effects from methods in https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement
data EffectEventTarget =
    AddEventListener EventTarget JSString Listener
    | RemoveEventListener EventTarget JSString Listener

instance (IsString str, Semigroup str) => ShowIO str EffectEventTarget where
    showsPrecIO p (AddEventListener j n _) = showParenIO (p >= 11) $ pure ((showStr "AddEventListener ") . (showsStr j) . (showsStr n))
    showsPrecIO p (RemoveEventListener j n _) = showParenIO (p >= 11) $ pure ((showStr "RemoveEventListener ") . (showsStr j) . (showsStr n))

addEventListener ::
    (CmdEventTarget c, HasCallStack, MonadReactor c m)
    => EventTarget
    -> JSString
    -> Listener
    -> m ()
addEventListener j n cb = loggedJS exec TRACE $ AddEventListener j n cb

removeEventListener ::
    (CmdEventTarget c, HasCallStack, MonadReactor c m)
    => EventTarget
    -> JSString
    -> Listener
    -> m ()
removeEventListener j n cb = loggedJS exec TRACE $ RemoveEventListener j n cb

-- | Add a listener with an event target, and automatically removes it on widget destruction
listenEventTarget ::
    (CmdEventTarget c, HasCallStack, NFData a, AskConstructor c m, AskDestructor c m, MonadReactor c m, AskPlanWeakRef m)
    => EventTarget -> JSString -> (JSVal -> MaybeT IO a) -> (a -> m ()) -> m ()
listenEventTarget j n goStrict goLazy = do
    onConstruction $ do
        hdl <- mkHandler goStrict goLazy
        cb <- mkListener hdl
        addEventListener j n cb
        onDestruction $ removeEventListener j n cb
