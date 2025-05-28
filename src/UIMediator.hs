{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UIMediator where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.Fix
import Debug.Trace (traceEventIO)
import GHC.Conc (labelThread)
import System.IO.Error (isEOFError, isIllegalOperation)
import UIInterface

data MedHandle = MedHandle
  { enqueueWrite :: String -> IO ()
  , blockingRead :: String -> IO String
  , openReadChannel :: String -> IO (TQueue String)
  , closeReadChannel :: IO ()
  , cleanUp :: IO ()
  }

data UIState = OpenChannel String | ClosedChannel | Exit deriving (Eq)
data BlockingPrompt = BlockingPrompt String (TMVar String)
newMediator :: UI -> IO (MedHandle, IO ())
newMediator (UI ui) = do
  writeQueue :: TQueue String <- atomically newTQueue
  readQueue :: TQueue String <- atomically newTQueue
  blockingQueue :: TQueue BlockingPrompt <- atomically newTQueue
  uiState <- atomically $ newTVar ClosedChannel
  let enqueueWrite :: String -> IO ()
      enqueueWrite = atomically . writeTQueue writeQueue
      reader :: IO ()
      reader = forever $ do
        join . atomically $ do
          readState <- (Left <$> (readTQueue blockingQueue)) `orElse` (Right <$> (readTVar uiState))
          case readState of
            Left (BlockingPrompt prompt readVar) -> return $ do
              res <- readUIWithPrompt ui prompt
              atomically $ writeTMVar readVar res
            Right (OpenChannel prompt) -> return $ do
              res <- readUIWithPrompt ui prompt
              atomically $ writeTQueue readQueue res
            Right ClosedChannel -> retry
            Right Exit -> pure $ pure ()

      blockingRead prompt = do
        resVar <- atomically $ do
          newVar <- newEmptyTMVar
          writeTQueue blockingQueue $ BlockingPrompt prompt newVar
          return newVar
        traceEventIO "Waiting on Mvar"
        res <- atomically $ do
          takeTMVar resVar `orElse` (readTVar uiState >>= check . (== Exit) >> return "")
        traceEventIO "After MVar"
        return res

      writer :: IO ()
      writer = forever $ do
        text <- atomically $ readTQueue writeQueue
        writeUI ui text
      openReadChannel :: String -> IO (TQueue String)
      openReadChannel prompt = do
        atomically $ writeTVar uiState $ OpenChannel prompt
        return readQueue
      closeReadChannel :: IO ()
      closeReadChannel = atomically $ writeTVar uiState ClosedChannel

      uiThread = mask $ \restore ->
        try (restore $ race_ reader writer)
          >>= ( \res -> do
                  atomically $ writeTVar uiState Exit
                  case res of
                    Left err -> case fromException err of
                      Just (ioe :: IOException) | isEOFError ioe -> putStrLn "EOF: Shutting down the ui thread"
                      Just (ioe :: IOException) | isIllegalOperation ioe -> putStrLn "Illegal operation: Shutting down the ui thread"
                      _ -> throw err
                    Right () -> print "Finishing gracefully"
              )
  let cleanUp = putStrLn "Cleaning up" >> (atomically $ writeTVar uiState Exit)
  return (MedHandle{..}, uiThread)

withMediator :: UI -> (MedHandle -> IO ()) -> IO ()
withMediator ui action = do
  (medHandle, uiThread) <- newMediator ui
  race_ uiThread (action medHandle) `finally` (cleanUp medHandle)
