module BoTox.Commands where

import           BoTox.Types
import qualified Data.ByteString.Char8 as BS
import           Network.Tox.C

runCommand :: ToxState -> Config -> Command -> IO ()
runCommand tox _cfg (CmdSelfSetStatus status) = do
  _ <- toxSelfSetStatus tox status
  return ()
                                           
runCommand tox cfg (CmdSelfSetStatusMessage statusMessage) = do
  _ <- toxSelfSetStatusMessage tox statusMessage
  savedata <- toxGetSavedata tox
  BS.writeFile (toxSavedataFilename cfg) savedata

runCommand tox cfg (CmdSelfSetNospam nospam) = do
  _ <- toxSelfSetNospam tox (fromIntegral nospam)
  savedata <- toxGetSavedata tox
  BS.writeFile (toxSavedataFilename cfg) savedata

runCommand tox cfg (CmdSelfSetName name) = do
  _ <- toxSelfSetName tox name
  savedata <- toxGetSavedata tox
  BS.writeFile (toxSavedataFilename cfg) savedata

runCommand tox cfg (CmdFriendDelete fn) = do
  _ <- toxFriendDelete tox (fromIntegral fn)
  savedata <- toxGetSavedata tox
  BS.writeFile (toxSavedataFilename cfg) savedata

runCommand tox cfg (CmdFriendAdd addr msg) = do
  _ <- toxFriendAdd tox addr msg
  savedata <- toxGetSavedata tox
  BS.writeFile (toxSavedataFilename cfg) savedata

runCommand tox cfg (CmdFriendAddNorequest addr) = do
  _ <- toxFriendAddNorequest tox addr
  savedata <- toxGetSavedata tox
  BS.writeFile (toxSavedataFilename cfg) savedata

runCommand tox _cfg (CmdFriendSendMessage fn msgType msg) = do
  _ <- toxFriendSendMessage tox (fromIntegral fn) msgType msg
  return ()

runCommand tox _cfg (CmdConferenceNew _) = do
  _ <- toxConferenceNew tox
  return ()

runCommand tox _cfg (CmdConferenceDelete cn) = do
  _ <- toxConferenceDelete tox (fromIntegral cn)
  return ()

runCommand tox _cfg (CmdConferenceInvite fn cn) = do
  _ <- toxConferenceInvite tox (fromIntegral fn) (fromIntegral cn)
  return ()

runCommand tox _cfg (CmdConferenceJoin fn cookie) = do
  _ <- toxConferenceJoin tox (fromIntegral fn) cookie
  return ()

runCommand tox _cfg (CmdConferenceSetTitle cn title) = do
  _ <- toxConferenceSetTitle tox (fromIntegral cn) title
  return ()

runCommand tox _cfg (CmdConferenceSendMessage cn msgType msg) = do
  _ <- toxConferenceSendMessage tox (fromIntegral cn) msgType msg
  return ()
