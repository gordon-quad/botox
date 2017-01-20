{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module BoTox.Types ( Friend(..)
                   , Conference(..)
                   , Peer(..)
                   , ToxAddress
                   , Cookie
                   , Message
                   , StatusMessage
                   , Title
                   , Name
                   , Nospam
                   , EventType (..)
                   , Event
                   , GroupEventType (..)
                   , GroupEvent
                   , Command (..)
                   , GroupCommand (..)
                   , Commands (..)
                   , GroupCommands (..) 
                   , ToxBotRState (..)
                   , ToxBotApp (..)
                   , MonadTB (..)
                   , ToxData (..)
                   , ToxState
                   , ToxBot
                   , GroupBot
                   , BootstrapNode (..)
                   , Config (..)
                   , readConfig
                   , runToxBotApp
                   , perGroup
                   , isMaster
                   , toxKeyFromString
                   ) where

import           Control.Auto
import           Control.Monad.Reader
import           Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import           Data.Either.Extra
import           Data.Maybe
import           Data.String
import           Data.Yaml
import           GHC.Generics
import           Network.Tox.C.Callbacks
import           Network.Tox.C.Tox
import           Network.Tox.C.Type (Tox)
import           Prelude hiding ((.), id)
import           System.Log.Logger
import           System.Posix.Time (epochTime)
import           System.Posix.Types (EpochTime)

  
data ToxData = ToxData [Event] Config
  deriving (Show)

zipMaybe :: (Maybe a, Maybe b) -> Maybe (a, b)
zipMaybe (a, b) = (,) <$> a <*> b


instance CHandler ToxData where
  cSelfConnectionStatus _tox conn (ToxData events cfg) = do
    time <- epochTime
    return $ ToxData ((time, EvtSelfConnectionStatus conn) : events) cfg
    
  cFriendName tox fn name td@(ToxData events cfg) = do
    time <- epochTime
    friend <- fillFriend tox cfg (fromIntegral fn)
    return $ maybe td (\f -> ToxData ((time, EvtFriendName f name) : events) cfg) friend
    
  cFriendStatusMessage tox fn statusMessage td@(ToxData events cfg) = do
    time <- epochTime
    friend <- fillFriend tox cfg (fromIntegral fn)
    return $ maybe td (\f -> ToxData ((time, EvtFriendStatusMessage f statusMessage) : events) cfg) friend
  
  cFriendStatus tox fn status td@(ToxData events cfg) = do
    time <- epochTime
    friend <- fillFriend tox cfg (fromIntegral fn)
    return $ maybe td (\f -> ToxData ((time, EvtFriendStatus f status) : events) cfg) friend
  
  cFriendConnectionStatus tox fn status td@(ToxData events cfg) = do
    time <- epochTime
    friend <- fillFriend tox cfg (fromIntegral fn)
    return $ maybe td (\f -> ToxData ((time, EvtFriendConnectionStatus f status) : events) cfg) friend
  
  cFriendRequest _tox addr msg (ToxData events cfg) = do
    time <- epochTime
    return $ ToxData ((time, EvtFriendRequest addr msg) : events) cfg
  
  cFriendMessage tox fn msgType msg td@(ToxData events cfg) = do
    time <- epochTime
    friend <- fillFriend tox cfg (fromIntegral fn)
    return $ maybe td (\f -> ToxData ((time, EvtFriendMessage f msgType msg) : events) cfg) friend
  
  cConferenceInvite tox fn confType cookie td@(ToxData events cfg) = do
    time <- epochTime
    friend <- fillFriend tox cfg (fromIntegral fn)
    return $ maybe td (\f -> ToxData ((time, EvtConferenceInvite f confType cookie) : events) cfg) friend
  
  cConferenceMessage tox cn pn msgType msg td@(ToxData events cfg) = do
    time <- epochTime
    conf <- fillConference tox (fromIntegral cn)
    peer <- fillPeer tox (fromIntegral cn) (fromIntegral pn)
    isSelf <- (liftM eitherToMaybe) $ toxConferencePeerNumberIsOurs tox (fromIntegral cn) (fromIntegral pn)
    case isSelf of
      Nothing -> return td
      Just False -> return $ maybe td (\(c, p) -> ToxData ((time, EvtConferenceMessage c p msgType msg) : events) cfg) $ zipMaybe (conf,peer)
      Just True -> return $ maybe td (\c -> ToxData ((time, EvtConferenceSelfMessage c msgType msg) : events) cfg) $ conf
  
  cConferenceTitle tox cn pn title td@(ToxData events cfg) = do
    time <- epochTime
    conf <- fillConference tox (fromIntegral cn)
    peer <- fillPeer tox (fromIntegral cn) (fromIntegral pn)
    return $ maybe td (\(c, p) -> ToxData ((time, EvtConferenceTitle c p title) : events) cfg) $ zipMaybe (conf,peer)
  
  cConferenceNamelistChange tox cn _pn _change td@(ToxData events cfg) = do
    time <- epochTime
    conf <- fillConference tox (fromIntegral cn)
    peerCount <- (liftM eitherToMaybe) $ toxConferencePeerCount tox (fromIntegral cn)
    peers <- case peerCount of
               Nothing -> return Nothing
               Just pc -> (liftM sequence) $ resolvePeers [1..(fromIntegral pc)]
    return $ maybe td (\(c, p) -> ToxData ((time, EvtConferenceNamelistChange c p) : events) cfg) $ zipMaybe (conf,peers)
    where
      resolvePeers = mapM (fillPeer tox (fromIntegral cn))


fillFriend :: ToxState -> Config -> Int -> IO (Maybe Friend)
fillFriend tox cfg num = do
  name <- (liftM eitherToMaybe) $ toxFriendGetName tox (fromIntegral num)
  pk <- (liftM eitherToMaybe) $ toxFriendGetPublicKey tox (fromIntegral num)
  connStatus <- (liftM eitherToMaybe) $ toxFriendGetConnectionStatus tox (fromIntegral num)
  statusMessage <- (liftM eitherToMaybe) $ toxFriendGetStatusMessage tox (fromIntegral num)
  return $ Friend <$> Just num <*> name <*> pk <*> connStatus <*> statusMessage <*> (isMaster cfg <$> pk)
                

data Friend = Friend { friendNum :: Int
                     , friendName :: Name
                     , friendPk :: ToxPublicKey
                     , friendConnectionStatus :: Connection
                     , friendStatusMessage :: StatusMessage
                     , friendIsMaster :: Bool }
  deriving (Eq, Show, Read)


fillConference :: ToxState -> Int -> IO (Maybe Conference)
fillConference tox num = do
  title <- (liftM eitherToMaybe) $ toxConferenceGetTitle tox (fromIntegral num)
  return $ Just (Conference num title)

data Conference = Conference { conferenceNum :: Int
                             , conferenceTitle :: Maybe Title }
  deriving (Eq, Show, Read)


fillPeer :: ToxState -> Int -> Int -> IO (Maybe Peer)
fillPeer tox cnum pnum = do
  name <- (liftM eitherToMaybe) $ toxConferencePeerGetName tox (fromIntegral cnum) (fromIntegral pnum)
  pk <- (liftM eitherToMaybe) $ toxConferencePeerGetPublicKey tox (fromIntegral cnum) (fromIntegral pnum)
  return $ Peer <$> Just pnum <*> name <*> pk


data Peer = Peer { peerNum :: Int
                 , peerName :: Name
                 , peerPk :: ToxPublicKey }
  deriving (Eq, Show, Read)


type ToxAddress = BS.ByteString
type ToxPublicKey = BS.ByteString
type Cookie = BS.ByteString
type Message = String
type StatusMessage = String
type Title = String
type Name = String
type Nospam = Int

data EventType = EvtSelfConnectionStatus      Connection
               | EvtFriendName                Friend Name
               | EvtFriendStatusMessage       Friend StatusMessage
               | EvtFriendStatus              Friend UserStatus
               | EvtFriendConnectionStatus    Friend Connection
               | EvtFriendRequest             ToxAddress Message
               | EvtFriendMessage             Friend MessageType Message
               | EvtConferenceInvite          Friend ConferenceType Cookie
               | EvtConferenceMessage         Conference Peer MessageType Message
               | EvtConferenceSelfMessage     Conference MessageType Message
               | EvtConferenceTitle           Conference Peer Title
               | EvtConferenceNamelistChange  Conference [Peer]
  deriving (Eq, Show)

type Event = (EpochTime, EventType)

data GroupEventType = EvtGroupMessage MessageType Message
  deriving Show

type GroupEvent = (EpochTime, GroupEventType)

data GroupCommand = CmdGroupMessage MessageType Message
  deriving Show

data Command = CmdSelfSetStatus             UserStatus
             | CmdSelfSetStatusMessage      StatusMessage
             | CmdSelfSetNospam             Nospam
             | CmdSelfSetName               Name
             | CmdFriendDelete              Int
             | CmdFriendAdd                 ToxAddress Message
             | CmdFriendAddNorequest        ToxAddress
             | CmdFriendSendMessage         Int MessageType Message
             | CmdConferenceNew             Int
             | CmdConferenceDelete          Int
             | CmdConferenceInvite          Int Int
             | CmdConferenceJoin            Int Cookie
             | CmdConferenceSetTitle        Int Title
             | CmdConferenceSendMessage     Int MessageType Message
  deriving (Eq, Show)
             
newtype Commands = Commands [Command] deriving Show
newtype GroupCommands = GroupCommands [GroupCommand] deriving Show

type ToxState = Tox ToxData

instance Monoid Commands where
  mempty = Commands []
  mappend (Commands m1) (Commands m2) = Commands (m1 ++ m2)
    
instance Monoid GroupCommands where
  mempty = GroupCommands []
  mappend (GroupCommands m1) (GroupCommands m2) = GroupCommands (m1 ++ m2)


type ToxBot m = Auto m Event Commands

type GroupBot m = Auto m GroupEvent GroupCommands

data ToxBotRState = ToxBotRState { toxState :: ToxState
                                 , config :: Config }

newtype ToxBotApp a = ToxBotA { runA :: ReaderT ToxBotRState IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader ToxBotRState)

class (MonadReader ToxBotRState m, MonadIO m, Monad m, Applicative m) => MonadTB m where
  tb :: ToxBotApp a -> m a
  getTox :: m ToxState
  getTox = asks toxState
  getConfig :: m Config
  getConfig = asks config


instance MonadTB ToxBotApp where tb = id


runToxBotApp :: ToxBotApp a -> ToxBotRState -> IO a
runToxBotApp = runReaderT . runA


perGroup :: Monad m => GroupBot m -> ToxBot m
perGroup gb = proc evts -> do
  gevtBlips <- emitJusts confMsg -< evts
  cmdBlips <- perBlip (confMap gb) -< gevtBlips
  cmds <- fromBlips mempty -< cmdBlips
  id -< cmds
  where
    confMsg (time, evt) = case evt of
                            EvtConferenceMessage cnf _ msgType msg -> 
                              Just (cnf, (time, EvtGroupMessage msgType msg))
                            _ -> Nothing

    toCmd conf (CmdGroupMessage msgType msg) = CmdConferenceSendMessage (conferenceNum conf) msgType msg

    confMap gbot = proc (conf, evt) -> do
      GroupCommands gcmds <- gbot -< evt
      let cmds = map (toCmd conf) gcmds
      id -< Commands cmds

data BootstrapNode = BootstrapNode { bootstrapAddress :: String
                                   , bootstrapPort :: Int
                                   , bootstrapPublicKey :: ToxPublicKey }
                     deriving (Show, Generic)

instance FromJSON BootstrapNode where
  parseJSON (Object v) =
    BootstrapNode <$>
    v .: "bootstrap-address" <*>
    v .: "bootstrap-port" <*>
    (toxKeyFromString <$> v .: "bootstrap-public-key")
  parseJSON _ = fail "Expected Object for BootstrapNode value"

data Config = Config { botName :: Name
                     , masterKeys :: [ToxPublicKey]
                     , bootstrapNodes :: [BootstrapNode]
                     , toxSavedataFilename :: String
                     , logFile :: FilePath
                     , logLevel :: Priority
                     , logFormat :: String}
              deriving (Show, Generic)

instance FromJSON Config where
  parseJSON (Object v) =
    Config <$>
    v .: "bot-name" <*>
    (map toxKeyFromString <$> v .: "master-keys") <*>
    v .: "bootstrap-nodes" <*>
    v .: "tox-savedata-filename" <*>
    v .: "log-file" <*>
    (read <$> v .: "log-level") <*>
    v .: "log-format"
  parseJSON _ = fail "Expected Object for Config value"

readConfig :: IO (Maybe Config)
readConfig = decodeFile "config.yaml"

isMaster :: Config -> BS.ByteString -> Bool
isMaster cfg key = elem key (masterKeys cfg)

toxKeyFromString :: String -> BS.ByteString
toxKeyFromString = fst . Base16.decode . fromString
