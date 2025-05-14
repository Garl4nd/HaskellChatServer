{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module TerminalUI (newTerminalUI) where

import Control.Concurrent.STM
import System.Console.ANSI
import System.IO
import UIInterface

data TerminalUI = TerminalUI {inputLock :: TMVar (), handle :: Handle, prompt :: String}
instance IsUI TerminalUI where
  setupUI = setupTerminalUI
  writeUI = writeToTerminal
  readUI = readTerminalInput
  readUIWithPrompt = readTerminalInputWithPrompt
  readCleanUpUI = cleanTerminalInput
  cleanupUI = hClose . handle

newTerminalUI :: Handle -> String -> IO TerminalUI
newTerminalUI handle prompt = do
  inputLock <- atomically $ newTMVar ()
  return TerminalUI{..}

lockTerminal :: TerminalUI -> IO ()
lockTerminal = const $ return () -- atomically . takeTMVar . inputLock

unlockTerminal :: TerminalUI -> IO ()
unlockTerminal = const $ return () -- atomically . flip putTMVar () . inputLock

readTerminalInput :: TerminalUI -> IO String
readTerminalInput tui@TerminalUI{..} = do
  lockTerminal tui
  result <- hGetLine handle
  cleanTerminalInput tui
  unlockTerminal tui
  return result

readTerminalInputWithPrompt :: String -> TerminalUI -> IO String
readTerminalInputWithPrompt prompt tui@TerminalUI{handle} = do
  lockTerminal tui
  hPutStr handle prompt
  hFlush handle
  res <- hGetLine handle
  cleanTerminalInput tui
  unlockTerminal tui
  return res

cleanTerminalInput :: TerminalUI -> IO ()
cleanTerminalInput TerminalUI{..} = do
  hCursorUpLine handle 1
  hClearLine handle

setupTerminalUI :: TerminalUI -> IO ()
setupTerminalUI TerminalUI{handle} = do
  hSetNewlineMode handle universalNewlineMode
  hSetBuffering handle LineBuffering

writeToTerminal :: String -> TerminalUI -> IO ()
writeToTerminal text tui@TerminalUI{handle, prompt} = do
  lockTerminal tui
  hCursorUpLine handle 0
  hClearLine handle
  hPutStrLn handle text
  hPutStr handle prompt
  hFlush handle
  unlockTerminal tui
