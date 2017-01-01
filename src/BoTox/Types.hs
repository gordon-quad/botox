{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module BoTox.Types where

import Control.Auto
import Control.Monad.Reader
import Data.ByteString (ByteString)
import Network.Tox.C.Tox (UserStatus(..), MessageType(..),
                          Connection(..), ConferenceStateChange(..),
                          ConferenceType(..))
import Network.Tox.C.Type (Tox)
import System.Posix.Types (EpochTime)

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

data EventType = EvtStartup
               | EvtSelfConnectionStatus      Connection
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

instance Monoid Commands where
  mempty = Commands []
  mappend (Commands m1) (Commands m2) = Commands (m1 ++ m2)
    
instance Monoid GroupCommands where
  mempty = GroupCommands []
  mappend (GroupCommands m1) (GroupCommands m2) = GroupCommands (m1 ++ m2)
    
type ToxBot m = Auto m Event Commands

type GroupBot m = Auto m GroupEvent GroupCommands

newtype ToxBotApp t a = ToxBotA { runA :: ReaderT (Tox t) IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (Tox t))

runToxBotApp :: ToxBotApp t a -> Tox t -> IO a
runToxBotApp a tox = runReaderT (runA a) tox

