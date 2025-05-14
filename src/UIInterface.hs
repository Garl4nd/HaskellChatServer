{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module UIInterface where

class IsUI u where
  setupUI :: u -> IO ()
  writeUI :: String -> u -> IO ()
  readUI :: u -> IO String
  readUIWithPrompt :: String -> u -> IO String
  readCleanUpUI :: u -> IO ()
  cleanupUI :: u -> IO ()

data UI where
  UI :: (IsUI u) => u -> UI

withUI :: UI -> (forall ui. (IsUI ui) => ui -> r) -> r
withUI (UI u) f = f u
