module BoTox.Commands where

import           BoTox.Types
import qualified Data.BoTox.Config as Cfg
import qualified Data.ByteString.Char8 as BS
import           Network.Tox.C

runCommand :: ToxState -> Command -> IO ()
runCommand tox (CmdSelfSetStatus status) = do
  _ <- toxSelfSetStatus tox status
  return ()
                                           
runCommand tox (CmdSelfSetStatusMessage statusMessage) = do
  _ <- toxSelfSetStatusMessage tox statusMessage
  savedata <- toxGetSavedata tox
  BS.writeFile Cfg.toxSavedataFilename savedata
  return ()

runCommand tox (CmdSelfSetNospam nospam) = do
  _ <- toxSelfSetNospam tox (fromIntegral nospam)
  savedata <- toxGetSavedata tox
  BS.writeFile Cfg.toxSavedataFilename savedata
  return ()

runCommand tox (CmdSelfSetName name) = do
  _ <- toxSelfSetName tox name
  savedata <- toxGetSavedata tox
  BS.writeFile Cfg.toxSavedataFilename savedata
  return ()

runCommand tox (CmdFriendDelete (Friend fn)) = do
  _ <- toxFriendDelete tox (fromIntegral fn)
  savedata <- toxGetSavedata tox
  BS.writeFile Cfg.toxSavedataFilename savedata
  return ()

runCommand tox (CmdFriendAdd addr msg ) = do
  _ <- toxFriendAdd tox addr msg
  savedata <- toxGetSavedata tox
  BS.writeFile Cfg.toxSavedataFilename savedata
  return ()

runCommand tox (CmdFriendAddNorequest addr ) = do
  _ <- toxFriendAddNorequest tox addr
  savedata <- toxGetSavedata tox
  BS.writeFile Cfg.toxSavedataFilename savedata
  return ()

runCommand tox (CmdFriendSendMessage (Friend fn) msgType msg) = do
  _ <- toxFriendSendMessage tox (fromIntegral fn) msgType msg
  return ()

runCommand tox (CmdConferenceNew _) = do
  _ <- toxConferenceNew tox
  return ()

runCommand tox (CmdConferenceDelete (Conference cn)) = do
  _ <- toxConferenceDelete tox (fromIntegral cn)
  return ()

runCommand tox (CmdConferenceInvite (Friend fn) (Conference cn)) = do
  _ <- toxConferenceInvite tox (fromIntegral fn) (fromIntegral cn)
  return ()

runCommand tox (CmdConferenceJoin (Friend fn) cookie) = do
  _ <- toxConferenceJoin tox (fromIntegral fn) cookie
  return ()

runCommand tox (CmdConferenceSetTitle (Conference cn) title) = do
  _ <- toxConferenceSetTitle tox (fromIntegral cn) title
  return ()

runCommand tox (CmdConferenceSendMessage (Conference cn) msgType msg) = do
  _ <- toxConferenceSendMessage tox (fromIntegral cn) msgType msg
  return ()
