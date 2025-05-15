{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ChatServer (runServer) where

import Control.Concurrent (forkFinally)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception hiding (handle)
import Control.Monad
import Data.Function (fix)
import Data.List (intercalate)
import qualified Data.Map as M
import NetworkUtils
import TerminalUI
import Text.Printf
import UIInterface

data Result = OK | Fail String

data BroadcastMessage = PublicNotice String | PublicMessage ClientID String
data SingleUserMessage = PrivateNotice String | PrivateMessage ClientID String
data Command = Perform String | ShowPrivate SingleUserMessage | ShowPublic BroadcastMessage

data ClientState = Logged | Kicked String deriving (Show)
data ServerState = ServerOn | ServerOff deriving (Eq)
data Client = Client {clId :: ClientID, clUI :: UI, clNickname :: TVar (Maybe String), clIsAdmin :: TVar Bool, clMessages :: TChan String, clPrivateCommands :: TChan Command, clState :: TVar ClientState, clPublicCommands :: TChan Command}
data Server = Server {clMap :: TVar (M.Map ClientID Client), broadcastChannel :: TChan Command, serverState :: TVar ServerState}
type ClientID = String

newServer :: IO Server
newServer = atomically $ do
  clMap <- newTVar M.empty
  broadcastChannel <- newTChan
  serverState <- newTVar ServerOn
  return Server{..}

createNewClient :: Server -> ClientID -> UI -> STM Client
createNewClient Server{clMap, broadcastChannel} clId clUI = do
  clPrivateCommands <- newTChan
  clState <- newTVar Logged
  clPublicCommands <- dupTChan broadcastChannel
  clients <- readTVar clMap
  clIsAdmin <- newTVar (M.null clients)
  clMessages <- newTChan
  clNickname <- newTVar Nothing
  return Client{..}

checkAndAddClient :: Server -> ClientID -> UI -> STM (Maybe Client)
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
  ensureExistsAdmin
 where
  ensureExistsAdmin = do
    clients <- readTVar clMap
    case M.elems clients of
      [] -> return ()
      (client@Client{clIsAdmin} : _) -> do
        admins <- getAdminList server
        when (null admins) $ do
          writeTVar (clIsAdmin) True
          name <- getDisplayName client
          broadcastNotice server $ printf "%s is now admin" name

runClient :: Server -> Client -> IO ()
runClient server client@Client{..} = race_ serverThread uiThread -- uiThread
 where
  serverThread = join . atomically $ do
    userState <- readTVar clState
    case userState of
      Kicked reason -> return $ withUI clUI $ \ui -> writeUI ui (printf "You have been kicked! Reason: %s" reason)
      Logged -> return $
        do
          command <- atomically $ readTChan clPrivateCommands `orElse` readTChan clPublicCommands
          continue <- atomically $ handleCommand server client command
          when (continue) serverThread

  uiThread = do
    inputChannel :: TChan (String) <- atomically $ newTChan
    withUI clUI $ \ui -> do
      let receiver = forever $ do
            input <- readUI ui
            atomically $ writeTChan inputChannel input
          dispatcher = forever $ do
            action <- atomically $ (Left <$> (readTChan clMessages)) `orElse` (Right <$> (readTChan inputChannel))
            case action of
              Left message -> writeUI ui message
              Right input -> atomically $ sendPrivateCommand client (Perform input)
      withAsync receiver $ \_ -> dispatcher

getDisplayName :: Client -> STM String
getDisplayName Client{clId, clNickname} = do
  nickname <- readTVar clNickname
  return $ case nickname of
    Just nick -> printf "%s (%s)" clId nick
    Nothing -> clId

broadcastCommand :: Server -> Command -> STM ()
broadcastCommand Server{broadcastChannel} cmd = writeTChan broadcastChannel cmd -- (ShowPublic msg)

broadcastNotice :: Server -> String -> STM ()
broadcastNotice server notice = broadcastCommand server $ ShowPublic (PublicNotice notice)

broadcastMessage :: Server -> Client -> String -> STM ()
broadcastMessage server client msg = getDisplayName client >>= \name -> broadcastCommand server $ ShowPublic (PublicMessage name msg)

sendPrivateCommand :: Client -> Command -> STM ()
sendPrivateCommand Client{clPrivateCommands} cmd = writeTChan clPrivateCommands cmd

sendPrivateNotice :: Client -> String -> STM ()
sendPrivateNotice client msg = sendPrivateCommand client (ShowPrivate $ PrivateNotice msg)

sendPrivateMessage :: Server -> Client -> ClientID -> String -> STM ()
sendPrivateMessage Server{clMap} client@Client{clId} toId msg = do
  clients <- readTVar clMap
  case M.lookup toId clients of
    Nothing -> sendPrivateNotice client $ printf "User %s is not logged in!" toId
    Just target -> getDisplayName client >>= \name -> sendPrivateCommand target (ShowPrivate $ PrivateMessage name msg)

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
makeNewAdmin server issuer@Client{clId = issuerId} targetId = performAdminAction server issuer (Just targetId) $ \target ->
  case target of
    Just Client{clIsAdmin} -> do
      writeTVar clIsAdmin True
      broadcastNotice server (printf "User %s has been promoted to admin by %s" targetId issuerId)
      return OK
    Nothing -> return $ Fail (printf "User %s not found" targetId)

changeNickname :: Server -> Client -> ClientID -> String -> STM ()
changeNickname server issuer@Client{clId = issuerId} targetId nickname = performAdminAction server issuer (Just targetId) $ \target ->
  case target of
    Just client@Client{clNickname} -> do
      oldDisplayName <- getDisplayName client
      writeTVar clNickname (Just nickname)
      broadcastNotice server (printf "%s changed %s's nickname to %s" issuerId oldDisplayName nickname)
      return OK
    Nothing -> return $ Fail (printf "User %s not found" targetId)

handleCommand :: Server -> Client -> Command -> STM Bool
handleCommand server client@Client{..} command = case command of
  (Perform "/quit") -> return False
  _ -> do
    case command of
      (Perform what) -> case words what of
        ("/tell" : to : msg) -> sendPrivateMessage server client to (unwords msg)
        ("/kick" : who : reason) -> kickUser server client who (unwords reason)
        (["/newadmin", who]) -> makeNewAdmin server client who
        ("/nickname" : who : nickname) -> changeNickname server client who (unwords nickname)
        ["/killserver"] -> performAdminAction server client Nothing (\_ -> OK <$ writeTVar (serverState server) ServerOff)
        ("/notice" : message) -> performAdminAction server client Nothing (\_ -> OK <$ broadcastNotice server (unwords message))
        ["/users"] -> getClientList server >>= \clientList -> sendPrivateNotice client (printf "Users: %s" $ intercalate ", " clientList)
        ["/admins"] -> getAdminList server >>= \adminList -> sendPrivateNotice client (printf "Admins: %s" $ intercalate ", " adminList)
        ('/' : _) : _ -> sendPrivateNotice client $ printf "Command %s not recognized" what
        strs | all null strs -> return ()
        _ -> broadcastMessage server client what
      ShowPrivate (PrivateMessage from msg) -> writeTChan clMessages $ printf "<*%s*> %s" from msg
      ShowPrivate (PrivateNotice msg) -> writeTChan clMessages $ printf "<*server*> %s" msg
      ShowPublic (PublicNotice msg) -> writeTChan clMessages $ printf "<server> %s" msg
      ShowPublic (PublicMessage from msg) -> writeTChan clMessages $ printf "<%s> %s" from msg -- printLine clHandle $ printf "<%s>: %s" from msg
    return True

runServer :: IO ()
runServer = withSocketsDo $ do
  server <- newServer
  port <- getPort "port.txt"
  listenOn port $ \sock -> do
    let checkQuit = do
          state <- readTVar (serverState server)
          check (state == ServerOff)
          broadcastNotice server "Killing server"
        acceptLoop = forever $ accept sock $ \(handle, peer) -> do
          printf "Accepted connection from %s\n" (show peer)
          tui <- UI <$> newTerminalUI handle "Text: "
          forkFinally (talk tui server) (\_ -> withUI tui cleanupUI)

    race_ (atomically checkQuit >> print "Killing server") acceptLoop

getPort :: FilePath -> IO Int
getPort filepath = read <$> readFile filepath

talk :: UI -> Server -> IO ()
talk clientUI server = withUI clientUI $ \ui -> do
  setupUI ui
  fix $ \loop -> do
    name <- readUIWithPrompt ui "Enter your name: "
    if null name
      then loop
      else mask $ \restore -> do
        addResult <- atomically $ checkAndAddClient server name clientUI
        case addResult of
          Nothing -> restore (writeUI ui (printf "User %s already logged on server! Choose a different name" name) >> loop)
          Just newClient ->
            restore (runClient server newClient) `finally` do
              writeUI ui "<server> Goodbye"
              removeClient server name
              printf "User %s disconnected\n" name
