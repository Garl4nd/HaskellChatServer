{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module TerminalUI (newTerminalUI) where

import Control.Concurrent.STM
import System.Console.ANSI
import System.IO
import UIInterface

data TerminalUI = TerminalUI {uiLock :: TMVar (), handle :: Handle, prompt :: String}
instance IsUI TerminalUI where
  setupUI = setupTerminalUI
  writeUI = writeToTerminal
  readUI = readTerminalInput
  readUIWithPrompt = readTerminalInputWithPrompt
  cleanupUI tui = writeToTerminal tui "Connection over" >> hClose (handle tui)

newTerminalUI :: Handle -> String -> IO TerminalUI
newTerminalUI handle prompt = do
  uiLock <- atomically $ newTMVar ()
  return TerminalUI{..}

lockTerminal :: TerminalUI -> IO ()
lockTerminal = atomically . takeTMVar . uiLock

unlockTerminal :: TerminalUI -> IO ()
unlockTerminal = atomically . flip putTMVar () . uiLock

readTerminalInput :: TerminalUI -> IO String
readTerminalInput tui@TerminalUI{..} = readTerminalInputWithPrompt tui prompt

readTerminalInputWithPrompt :: TerminalUI -> String -> IO String
readTerminalInputWithPrompt tui@TerminalUI{handle} prompt = do
  hPutStr handle prompt
  hFlush handle
  res <- hGetLine handle
  cleanTerminalInput tui
  return res

cleanTerminalInput :: TerminalUI -> IO ()
cleanTerminalInput TerminalUI{..} = do
  hCursorUpLine handle 1
  hClearLine handle

setupTerminalUI :: TerminalUI -> IO ()
setupTerminalUI TerminalUI{handle} = do
  hSetNewlineMode handle universalNewlineMode
  hSetBuffering handle LineBuffering

writeToTerminal :: TerminalUI -> String -> IO ()
writeToTerminal tui@TerminalUI{handle, prompt} text = do
  hCursorUpLine handle 0
  hClearLine handle
  hPutStrLn handle text
  hPutStr handle prompt
  hFlush handle
