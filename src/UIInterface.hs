{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module UIInterface where

class IsUI u where
  setupUI :: u -> IO ()
  writeUI :: u -> String -> IO ()
  readUI :: u -> IO String
  readUIWithPrompt :: u -> String -> IO String
  readCleanUpUI :: u -> IO ()
  cleanupUI :: u -> IO ()

data UI where
  UI :: (IsUI u) => u -> UI

withUI :: UI -> (forall ui. (IsUI ui) => ui -> r) -> r
withUI (UI u) f = f u

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
