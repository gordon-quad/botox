{-# LANGUAGE Arrows #-}
module BoTox.Bots.GroupBot where

import           BoTox.Bots.Utils
import           BoTox.Types

import           Control.Auto
import           Control.Auto.Blip
import           Control.Monad.Reader
import qualified Data.ByteString as BS
import           Data.Map as M
import           Network.Tox.C
import           Prelude hiding ((.), id)
import           Text.Read (readMaybe)

groupBot :: MonadTB m => ToxBot m
groupBot = proc event@(_time, evt) -> do
  autoInviteMsgs <- perBlip resolvePk . parseFriendEventCmd ["autoinvite"] parseAutoInviteArgs -< event
  autoInvites <- updateAutoInvites -< autoInviteMsgs
  onlineFriends <- perBlip resolvePk . emitJusts friendOnlineEvent -< evt
  inviteMsgs <- parseFriendEventCmd ["invite"] parseInviteArgs -< event
  masterInviteConfs <- filterB isInviteEvent . masterEvents -< event
  friendRequests <- emitOn isFriendRequestEvent -< event

  let queryAutoInvites :: ((Friend, BS.ByteString), a) -> ((Friend, Bool), a)
      queryAutoInvites ((friend, pk), x) = ((friend, M.findWithDefault False pk autoInvites), x)
  
  aiCmds <- fromBlips mempty . modifyBlips doAutoInviteCmds -< queryAutoInvites <$> autoInviteMsgs
  foCmds <- fromBlips mempty . modifyBlips doFriendOnline -< queryAutoInvites <$> onlineFriends
  iCmds <- fromBlips mempty . modifyBlips doInvite -< inviteMsgs
  micCmds <- fromBlips mempty . modifyBlips doMasterJoin -< masterInviteConfs
  frCmds <- fromBlips mempty . modifyBlips doFriendAdd -< friendRequests

  id -< mconcat [aiCmds, foCmds, iCmds, micCmds, frCmds]
  where
    parseAutoInviteArgs "on"    = Just True
    parseAutoInviteArgs "On"    = Just True
    parseAutoInviteArgs "yes"   = Just True
    parseAutoInviteArgs "Yes"   = Just True
    parseAutoInviteArgs "true"  = Just True
    parseAutoInviteArgs "True"  = Just True
    parseAutoInviteArgs "off"   = Just False
    parseAutoInviteArgs "Off"   = Just False
    parseAutoInviteArgs "no"    = Just False
    parseAutoInviteArgs "No"    = Just False
    parseAutoInviteArgs "false" = Just False
    parseAutoInviteArgs "False" = Just False
    parseAutoInviteArgs _       = Nothing

    parseInviteArgs "" = Just 0
    parseInviteArgs a  = readMaybe a

    updateAutoInvites = scanB upd M.empty

    upd m ((_friend, key), Just val) = M.insert key val m
    upd m _                          = m

    friendOnlineEvent (EvtFriendConnectionStatus _ ConnectionNone) = Nothing
    friendOnlineEvent (EvtFriendConnectionStatus friend _)         = Just (friend, ())
    friendOnlineEvent _                                            = Nothing

    isInviteEvent (_, EvtConferenceInvite _ _ _) = True
    isInviteEvent _                              = False

    isFriendRequestEvent (_, EvtFriendRequest _ _) = True
    isFriendRequestEvent _                         = False

    doAutoInviteCmds ((friend, _), Just True) =
      botFriendSay friend "Auto-invite turned on"
    doAutoInviteCmds ((friend, _), Just False) =
      botFriendSay friend "Auto-invite turned off"
    doAutoInviteCmds ((friend, autoInvite), Nothing) =
      botFriendSay friend ("Auto-invite is " ++ (show autoInvite))

    doFriendOnline ((friend, True), _) =
        Commands [CmdConferenceInvite friend (Conference 0)]
    doFriendOnline ((_, False), _) =
        Commands []

    doInvite (friend, Just confNum) = Commands [CmdConferenceInvite friend (Conference confNum)]
    doInvite (_, Nothing)           = Commands []

    doMasterJoin (_, EvtConferenceInvite friend ConferenceTypeText cookie) =
      Commands [CmdConferenceJoin friend cookie]
    doMasterJoin _ =
      Commands []
  
    doFriendAdd (_, EvtFriendRequest addr _) =
      Commands [CmdFriendAddNorequest addr]
    doFriendAdd _ =
      Commands []
    
                                
resolvePk :: MonadTB m => Auto m (Friend, a) ((Friend, BS.ByteString), a)
resolvePk = proc (friend, args) -> do
  pk <- arrM getPk -< friend
  id -< ((friend, pk), args)
  where
    getFriendNum (Friend num) = fromIntegral num

    getPk friend = do
      t <- getTox
      pk <- liftIO $ toxFriendGetPublicKey t (getFriendNum friend)
      case pk of
        Left _err -> return BS.empty
        Right key -> return key

