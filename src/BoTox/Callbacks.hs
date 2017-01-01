module BoTox.Callbacks where

import BoTox.Types
import Network.Tox.C.Callbacks
import Data.Word ()
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
