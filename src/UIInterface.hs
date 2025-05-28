{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UIInterface where

import Control.Concurrent
import Control.Concurrent.Async (race_)
import Control.Concurrent.STM (TQueue, atomically, newTQueue, newTQueueIO, readTQueue, writeTQueue, writeTVar)
import Control.Exception
import Control.Monad (forever, when)
import Text.ParserCombinators.ReadPrec (reset)

class IsUI u where
  writeUI :: u -> String -> IO ()
  readUI :: u -> IO String
  readUIWithPrompt :: u -> String -> IO String
  cleanupUI :: u -> IO ()
  isValidUI :: u -> IO Bool

data UI where
  UI :: (IsUI u) => u -> UI

class UIOutput o where
  showUI :: o -> Maybe String

instance UIOutput () where
  showUI () = Nothing
instance UIOutput String where
  showUI = Just . id
instance UIOutput Int where
  showUI = Just . show

data SomeOutputAction where
  WrappedAction :: (UIOutput r) => UIAction r -> SomeOutputAction

type UIAction r = (forall ui. (IsUI ui) => ui -> IO r)
data UIHandle = UIHandle
  { uiRunner :: forall r. (UIOutput r) => UIAction r -> IO ()
  , uiReader :: String -> IO String
  , resultQueue :: TQueue String
  }

withUI :: UI -> UIAction r -> IO r
withUI (UI u) f = f u

--
-- launchUI :: UI -> IO (UIHandle)
-- launchUI (UI ui) = do
--   performQueue :: TQueue SomeOutputAction <- atomically $ newTQueue
--   resultQueue :: TQueue String <- atomically $ newTQueue
--   let enqueue :: (UIOutput r) => UIAction r -> IO ()
--       enqueue action = atomically . writeTQueue performQueue $ WrappedAction action
--       listener = forever $ do
--         WrappedAction action <- atomically $ readTQueue performQueue
--         p <- action ui
--         let res = Just ""
--         case res of
--           Just res -> atomically $ writeTQueue resultQueue res
--           _ -> return ()
--       reader prompt = readUIWithPrompt ui prompt
--   forkIO $ bracket (writeUI ui "Launching session") (\_ -> cleanupUI ui) $ \_ -> listener
--   return UIHandle{uiRunner = enqueue, uiReader = reader, resultQueue}
--
--   let listener = forever $ do
--         action <- atomically $ readTQueue queue
--         action ui
--   forkIO $ listener
--   return $ UIHandle (atomically . writeTQueue queue)
--

-- setup :: UI -> IO ()
-- setup = (`withUI` setupUI)
--
-- write :: UI -> String -> IO ()
-- write = (`withUI` writeUI)
--
-- read :: UI -> IO String
-- read = (`withUI` readUI)
--
-- readWithPrompt :: UI -> String -> IO String
-- readWithPrompt = (`withUI` readUIWithPrompt)
--
-- readCleanUp :: UI -> IO ()
-- readCleanUp = (`withUI` readCleanUpUI)
--
-- cleanup :: UI -> IO ()
-- cleanup = (`withUI` cleanupUI)
