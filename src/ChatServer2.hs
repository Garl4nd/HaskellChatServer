{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ChatServer2 (runServer2) where

import Control.Concurrent (forkFinally, forkIO)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception hiding (handle)
import Control.Monad
import Data.Function (fix)
import Data.List (intercalate)
import qualified Data.Map as M
import GHC.IO.Handle (hClose)
import NetworkUtils
import TerminalUI
import Text.Printf
import UIInterface
import UIMediator

data Result = OK | Fail String

data BroadcastMessage = PublicNotice String | PublicMessage ClientID String
data SingleUserMessage = PrivateNotice String | PrivateMessage ClientID String
data Command = Perform String | ShowPrivate SingleUserMessage | ShowPublic BroadcastMessage

data ClientState = Logged | Kicked String deriving (Show)
data ServerState = ServerOn | ServerOff deriving (Eq)
data Client = Client {clId :: ClientID, clUI :: MedHandle, clIsAdmin :: TVar Bool, clMessages :: TChan String, clPrivateCommands :: TChan Command, clState :: TVar ClientState, clPublicCommands :: TChan Command}
data Server = Server {clMap :: TVar (M.Map ClientID Client), broadcastChannel :: TChan Command, serverState :: TVar ServerState}
type ClientID = String

newServer :: IO Server
newServer = atomically $ do
  clMap <- newTVar M.empty
  broadcastChannel <- newTChan
  serverState <- newTVar ServerOn
  return Server{..}

createNewClient :: Server -> ClientID -> MedHandle -> STM Client
createNewClient Server{clMap, broadcastChannel} clId clUI = do
  clPrivateCommands <- newTChan
  clState <- newTVar Logged
  clPublicCommands <- dupTChan broadcastChannel
  clients <- readTVar clMap
  clIsAdmin <- newTVar (M.null clients)
  clMessages <- newTChan
  return Client{..}

checkAndAddClient :: Server -> ClientID -> MedHandle -> STM (Maybe Client)
checkAndAddClient server@Server{clMap} clId ui = do
  clients <- readTVar clMap
  if clId `elem` M.keys clients
    then return Nothing
    else do
      newClient <- createNewClient server clId ui
      modifyTVar clMap (M.insert clId newClient)
      broadcastNotice server $ printf "Client %s joined the chat" clId
      return $ Just newClient

removeClient :: Server -> ClientID -> IO ()
removeClient server@Server{..} clId = atomically $ do
  modifyTVar clMap (M.delete clId)
  broadcastNotice server (printf "User %s left the chat" clId)

runClient :: Server -> Client -> IO ()
runClient server client@Client{..} = race_ serverThread receiverThread `finally` (print "bam")
 where
  receiverThread = do
    readQueue <- openReadChannel clUI "Text: "
    forever . atomically $ do
      input <- readTQueue readQueue
      sendPrivateCommand client (Perform input)

  serverThread = join . atomically $ do
    userState <- readTVar clState
    case userState of
      Kicked reason -> return $ enqueueWrite clUI $ printf "You have been kicked! Reason: %s" reason -- withUI clUI $ \ui -> writeUI ui (printf "You have been kicked! Reason: %s" reason)
      Logged -> return $
        do
          command <- atomically $ readTChan clPrivateCommands `orElse` readTChan clPublicCommands
          continue <- handleCommand server client command
          when (continue) serverThread

broadcastCommand :: Server -> Command -> STM ()
broadcastCommand Server{broadcastChannel} cmd = writeTChan broadcastChannel cmd -- (ShowPublic msg)

broadcastNotice :: Server -> String -> STM ()
broadcastNotice server notice = broadcastCommand server $ ShowPublic (PublicNotice notice)

broadcastMessage :: Server -> ClientID -> String -> STM ()
broadcastMessage server clId msg = broadcastCommand server $ ShowPublic (PublicMessage clId msg)

sendPrivateCommand :: Client -> Command -> STM ()
sendPrivateCommand Client{clPrivateCommands} cmd = writeTChan clPrivateCommands cmd

sendPrivateNotice :: Client -> String -> STM ()
sendPrivateNotice client msg = sendPrivateCommand client (ShowPrivate $ PrivateNotice msg)

sendPrivateMessage :: Server -> Client -> ClientID -> String -> STM ()
sendPrivateMessage Server{clMap} client@Client{clId} toId msg = do
  clients <- readTVar clMap
  case M.lookup toId clients of
    Nothing -> sendPrivateNotice client $ printf "User %s is not logged in!" toId
    Just target -> sendPrivateCommand target (ShowPrivate $ PrivateMessage clId msg)

getClientList :: Server -> STM [ClientID]
getClientList Server{clMap} = M.keys <$> readTVar clMap

getAdminList :: Server -> STM [ClientID]
getAdminList Server{clMap} = do
  clients <- M.elems <$> readTVar clMap
  admins <- filterM (readTVar . clIsAdmin) clients
  return $ clId <$> admins

performAdminAction :: Server -> Client -> Maybe ClientID -> (Maybe Client -> STM Result) -> STM ()
performAdminAction Server{clMap} issuer target action = do
  isIssuerAdmin <- readTVar $ clIsAdmin issuer
  actionResult <-
    if (not isIssuerAdmin)
      then return $ Fail "Only admins can perform this action"
      else do
        case target of
          Nothing -> action Nothing
          Just targetId -> do
            clients <- readTVar clMap
            action $ M.lookup targetId clients
  sendPrivateNotice issuer $ case actionResult of
    OK -> "Action succesfull"
    Fail failReason -> printf "Action failed: %s" failReason

kickUser :: Server -> Client -> ClientID -> String -> STM ()
kickUser server kicker kickeeId reason = performAdminAction server kicker (Just kickeeId) $ \target ->
  if (null reason)
    then return (Fail "You must provide a reason for the kick")
    else case target of
      Just Client{..} -> do
        writeTVar clState (Kicked reason)
        broadcastNotice server (printf "User %s has been kicked. Reason: %s" clId reason)
        return OK
      Nothing -> return $ Fail (printf "User %s not found" kickeeId)

makeNewAdmin :: Server -> Client -> ClientID -> STM ()
makeNewAdmin server issuer@Client{clId = issuerId} targetId = performAdminAction server issuer (Just targetId) $ \target -> do
  case target of
    Just Client{clIsAdmin} -> do
      writeTVar clIsAdmin True
      broadcastNotice server (printf "User %s has been promoted to admin by %s" targetId issuerId)
      return OK
    Nothing -> return $ Fail (printf "User %s not found" targetId)

renameUser :: Server -> Client -> ClientID -> ClientID -> STM ()
renameUser server@Server{clMap} issuer oldName newName = performAdminAction server issuer (Just oldName) $ \target -> do
  case target of
    Just client -> modifyTVar clMap (M.insert newName client . M.delete oldName) >> return OK
    Nothing -> return $ Fail (printf "User %s not found" oldName)

handleCommand :: Server -> Client -> Command -> IO Bool
handleCommand server client@Client{..} command = case command of
  (Perform "/quit") -> write "<*server> Goodbye" >> return False
  _ -> do
    case command of
      (Perform what) -> atomically $ case words what of
        ("/tell" : to : msg) -> sendPrivateMessage server client to (unwords msg)
        ("/kick" : who : reason) -> kickUser server client who (unwords reason)
        ("/rename" : who : newName : _) -> renameUser server client who newName
        (["/newadmin", who]) -> do makeNewAdmin server client who
        ["/killserver"] -> performAdminAction server client Nothing (\_ -> OK <$ writeTVar (serverState server) ServerOff)
        ("/notice" : message) -> performAdminAction server client Nothing (\_ -> OK <$ broadcastNotice server (unwords message))
        ["/users"] -> getClientList server >>= \clientList -> sendPrivateNotice client (printf "Users: %s" $ intercalate ", " clientList)
        ["/admins"] -> getAdminList server >>= \adminList -> sendPrivateNotice client (printf "Admins: %s" $ intercalate ", " adminList)
        ('/' : _) : _ -> sendPrivateNotice client "Unrecognized command"
        strs | all null strs -> return ()
        _ -> broadcastMessage server clId what
      ShowPrivate (PrivateMessage from msg) -> write $ printf "<*%s*> %s" from msg
      ShowPrivate (PrivateNotice msg) -> write $ printf "<*server*> %s" msg
      ShowPublic (PublicNotice msg) -> write $ printf "<server> %s" msg
      ShowPublic (PublicMessage from msg) -> write $ printf "<%s> %s" from msg -- printLine clHandle $ printf "<%s>: %s" from msg
    return True
 where
  write :: String -> IO ()
  write = enqueueWrite clUI

runServer2 :: IO ()
runServer2 = withSocketsDo $ do
  server <- newServer
  port <- getPort
  listenOn port $ \sock -> do
    let checkQuit = do
          state <- readTVar (serverState server)
          check (state == ServerOff)
          broadcastNotice server "Killing server"
        acceptLoop = forever $ accept sock $ \(handle, peer) -> do
          printf "Accepted connection from %s\n" (show peer)
          tui <- UI <$> newTerminalUI handle "Text: "
          forkFinally
            (withMediator tui $ \handle -> talk handle server)
            ( \case
                Left (err :: SomeException) -> print err >> throw err
                Right () -> putStrLn "Cleaning up" >> withUI tui cleanupUI
            )

    race_ (atomically checkQuit >> print "Killing server") acceptLoop

getPort :: IO Int
getPort = read <$> readFile "port.txt"

talk :: MedHandle -> Server -> IO ()
talk uiHandle@MedHandle{..} server = fix $ \loop -> do
  name <- blockingRead "Enter your name: "
  if null name
    then loop
    else mask $ \restore -> do
      addResult <- atomically $ checkAndAddClient server name uiHandle
      case addResult of
        Nothing -> restore (enqueueWrite (printf "User %s already logged on server! Choose a different name" name) >> loop)
        Just newClient ->
          restore (runClient server newClient) `finally` do
            print "Finalizing"
            removeClient server name
            putStrLn $ printf "User %s disconnected" name
