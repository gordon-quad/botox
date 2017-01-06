{-# LANGUAGE GeneralizedNewtypeDeriving, Arrows, FlexibleContexts #-}

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
                   , runToxBotApp
                   , perGroup
                   ) where

import Control.Auto
import Control.Monad.Reader
import Data.ByteString (ByteString)
import Network.Tox.C.Tox (UserStatus(..), MessageType(..),
                          Connection(..), ConferenceStateChange(..),
                          ConferenceType(..))
import Network.Tox.C.Callbacks
import Network.Tox.C.Type (Tox)
import System.Posix.Types (EpochTime)
import Prelude hiding ((.), id)
import System.Posix.Time (epochTime)

  
newtype ToxData = ToxData [Event]
  deriving (Eq, Show)

  
instance CHandler ToxData where
  cSelfConnectionStatus _tox conn (ToxData events) = do
    time <- epochTime
    return $ ToxData ((time, EvtSelfConnectionStatus conn) : events)
    
  cFriendName _tox fn name (ToxData events) = do
    time <- epochTime
    let friend = Friend (fromIntegral fn)
    return $ ToxData ((time, EvtFriendName friend name) : events)
    
  cFriendStatusMessage _tox fn statusMessage (ToxData events) = do
    time <- epochTime
    let friend = Friend (fromIntegral fn)
    return $ ToxData ((time, EvtFriendStatusMessage friend statusMessage) : events)
  
  cFriendStatus _tox fn status (ToxData events) = do
    time <- epochTime
    let friend = Friend (fromIntegral fn)
    return $ ToxData ((time, EvtFriendStatus friend status) : events)
  
  cFriendConnectionStatus _tox fn status (ToxData events) = do
    time <- epochTime
    let friend = Friend (fromIntegral fn)
    return $ ToxData ((time, EvtFriendConnectionStatus friend status) : events)
  
  cFriendRequest _tox addr msg (ToxData events) = do
    time <- epochTime
    return $ ToxData ((time, EvtFriendRequest addr msg) : events)
  
  cFriendMessage _tox fn msgType msg (ToxData events) = do
    time <- epochTime
    let friend = Friend (fromIntegral fn)
    return $ ToxData ((time, EvtFriendMessage friend msgType msg) : events)
  
  cConferenceInvite _tox fn confType cookie (ToxData events) = do
    time <- epochTime
    let friend = Friend (fromIntegral fn)
    return $ ToxData ((time, EvtConferenceInvite friend confType cookie) : events)
  
  cConferenceMessage _tox cn pn msgType msg (ToxData events) = do
    time <- epochTime
    let conf = Conference (fromIntegral cn)
    let peer = Peer (fromIntegral pn)
    return $ ToxData ((time, EvtConferenceMessage conf peer msgType msg) : events)
  
  cConferenceTitle _tox cn pn title (ToxData events) = do
    time <- epochTime
    let conf = Conference (fromIntegral cn)
    let peer = Peer (fromIntegral pn)
    return $ ToxData ((time, EvtConferenceTitle conf peer title) : events)
  
  cConferenceNamelistChange _tox cn pn change (ToxData events) = do
    time <- epochTime
    let conf = Conference (fromIntegral cn)
    let peer = Peer (fromIntegral pn)
    return $ ToxData ((time, EvtConferenceNamelistChange conf peer change) : events)


newtype Friend = Friend Int
  deriving (Eq, Show, Read)

newtype Conference = Conference Int
  deriving (Eq, Show, Read)

newtype Peer = Peer Int
  deriving (Eq, Show, Read)

type ToxAddress = ByteString
type Cookie = ByteString
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
               | EvtConferenceTitle           Conference Peer Title
               | EvtConferenceNamelistChange  Conference Peer ConferenceStateChange
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
             | CmdFriendDelete              Friend
             | CmdFriendAdd                 ToxAddress Message
             | CmdFriendAddNorequest        ToxAddress
             | CmdFriendSendMessage         Friend MessageType Message
             | CmdConferenceNew             Conference
             | CmdConferenceDelete          Conference
             | CmdConferenceInvite          Friend Conference
             | CmdConferenceJoin            Friend Cookie
             | CmdConferenceSetTitle        Conference Title
             | CmdConferenceSendMessage     Conference MessageType Message
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

data ToxBotRState = ToxBotRState { toxState :: ToxState }

newtype ToxBotApp a = ToxBotA { runA :: ReaderT ToxBotRState IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader ToxBotRState)

class (MonadReader ToxBotRState m, MonadIO m, Monad m, Applicative m) => MonadTB m where
  tb :: ToxBotApp a -> m a
  getTox :: m ToxState
  getTox = asks toxState


instance MonadTB ToxBotApp where tb = id


runToxBotApp :: ToxBotApp a -> ToxBotRState -> IO a
runToxBotApp = runReaderT . runA


perGroup :: Monad m => GroupBot m -> ToxBot m
perGroup gb = proc evts -> do
  gevtBlips <- emitJusts confMsg -< evts
  cmdBlips <- perBlip (confMap gb) -< gevtBlips
  cmds <- fromBlips (Commands []) -< cmdBlips
  id -< cmds
  where
    confMsg (time, evt) = case evt of
                            EvtConferenceMessage cnf _ msgType msg -> 
                              Just (cnf, (time, EvtGroupMessage msgType msg))
                            _ -> Nothing

    toCmd conf (CmdGroupMessage msgType msg) = CmdConferenceSendMessage conf msgType msg

    confMap gbot = proc (conf, evt) -> do
      GroupCommands gcmds <- gbot -< evt
      let cmds = map (toCmd conf) gcmds
      id -< Commands cmds
