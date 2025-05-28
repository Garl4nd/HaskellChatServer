{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module TerminalUI (newTerminalUI) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (bracket)
import System.Console.ANSI
import System.IO
import UIInterface

data TerminalUI = TerminalUI {uiLock :: MVar (), handle :: Handle, prompt :: String}
instance IsUI TerminalUI where
  writeUI = writeToTerminal
  readUI = readTerminalInput
  readUIWithPrompt = readTerminalInputWithPrompt
  cleanupUI = hClose . handle
  isValidUI = fmap not . hIsClosed . handle

newTerminalUI :: Handle -> String -> IO TerminalUI
newTerminalUI handle prompt = do
  uiLock <- newMVar ()
  let termUI = TerminalUI{..}
  setupTerminalUI termUI
  return termUI

lockTerminal :: TerminalUI -> IO ()
lockTerminal = takeMVar . uiLock

unlockTerminal :: TerminalUI -> IO ()
unlockTerminal = flip putMVar () . uiLock

readTerminalInput :: TerminalUI -> IO String
readTerminalInput tui@TerminalUI{..} = readTerminalInputWithPrompt tui prompt

readTerminalInputWithPrompt :: TerminalUI -> String -> IO String
readTerminalInputWithPrompt tui@TerminalUI{handle} prompt = do
  bracket (lockTerminal tui) (const $ unlockTerminal tui) $ \_ -> do
    clearTerminalLine tui 0
    hPutStr handle prompt
    hFlush handle
  res <- hGetLine handle
  clearTerminalLine tui 1
  return res

clearTerminalLine :: TerminalUI -> Int -> IO ()
clearTerminalLine TerminalUI{..} n = do
  hCursorUpLine handle n
  hClearLine handle

setupTerminalUI :: TerminalUI -> IO ()
setupTerminalUI TerminalUI{handle} = do
  hSetNewlineMode handle universalNewlineMode
  hSetBuffering handle LineBuffering

writeToTerminal :: TerminalUI -> String -> IO ()
writeToTerminal tui@TerminalUI{handle, prompt} text = bracket (lockTerminal tui) (\_ -> unlockTerminal tui) $ \_ -> do
  hCursorUpLine handle 0
  hClearLine handle
  hPutStrLn handle text
  hPutStr handle prompt
  hFlush handle
