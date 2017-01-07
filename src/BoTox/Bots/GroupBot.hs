{-# LANGUAGE Arrows #-}
module BoTox.Bots.GroupBot where

import           BoTox.Types
import           BoTox.Utils

import           Control.Auto
import           Control.Auto.Blip
import           Data.Map as M
import           Network.Tox.C
import           Prelude hiding ((.), id)
import           Text.Parsec

groupBot :: MonadTB m => ToxBot m
groupBot = proc event@(_time, evt) -> do
  autoInviteMsgs <- parseFriendEventCmd ["autoinvite"] autoInviteArgsParser -< event
  onlineFriends <- emitJusts friendOnlineEvent -< evt
  inviteMsgs <- parseFriendEventCmd ["invite"] inviteArgsParser -< event
  masterInviteConfs <- filterB isInviteEvent . masterEvents -< event
  friendRequests <- emitOn isFriendRequestEvent -< event

  autoInvites <- updateAutoInvites -< autoInviteMsgs

  let queryAutoInvites :: (Friend, a) -> ((Friend, Bool), a)
      queryAutoInvites (friend@(Friend { friendPk = pk }), x) = ((friend, M.findWithDefault False pk autoInvites), x)
  
  aiCmds <- fromBlips mempty . modifyBlips doAutoInviteCmds -< queryAutoInvites <$> autoInviteMsgs
  foCmds <- fromBlips mempty . modifyBlips doFriendOnline -< queryAutoInvites <$> onlineFriends
  iCmds <- fromBlips mempty . modifyBlips doInvite -< inviteMsgs
  micCmds <- fromBlips mempty . modifyBlips doMasterJoin -< masterInviteConfs
  frCmds <- fromBlips mempty . modifyBlips doFriendAdd -< friendRequests

  id -< mconcat [aiCmds, foCmds, iCmds, micCmds, frCmds]
  where
    autoInviteArgsParser = optionMaybe bool

    inviteArgsParser = choice [try integer, try $ 0 <$ eof]
  
    updateAutoInvites = scanB upd M.empty

    upd m (Friend {friendPk = key}, Right (Just val)) = M.insert key val m
    upd m _                                  = m

    friendOnlineEvent (EvtFriendConnectionStatus _ ConnectionNone) = Nothing
    friendOnlineEvent (EvtFriendConnectionStatus friend _)         = Just (friend, ())
    friendOnlineEvent _                                            = Nothing

    isInviteEvent (_, EvtConferenceInvite _ _ _) = True
    isInviteEvent _                              = False

    isFriendRequestEvent (_, EvtFriendRequest _ _) = True
    isFriendRequestEvent _                         = False

    doAutoInviteCmds ((friend, _), Right (Just True)) =
      botFriendSay friend "Auto-invite turned on"
    doAutoInviteCmds ((friend, _), Right (Just False)) =
      botFriendSay friend "Auto-invite turned off"
    doAutoInviteCmds ((friend, autoInvite), Right Nothing) =
      botFriendSay friend $ "Auto-invite is " ++ (show autoInvite)
    doAutoInviteCmds ((friend, _), Left _) =
      botFriendSay friend "Usage: autoinvite [on|off]"

    doFriendOnline ((friend, True), _) =
        Commands [CmdConferenceInvite (friendNum friend) 0]
    doFriendOnline ((_, False), _) =
        mempty

    doInvite (friend, Right confNum) = Commands [CmdConferenceInvite (friendNum friend) confNum]
    doInvite (friend, Left _)      = botFriendSay friend "Usage: invite [conference]"

    doMasterJoin (_, EvtConferenceInvite friend ConferenceTypeText cookie) =
      Commands [CmdConferenceJoin (friendNum friend) cookie]
    doMasterJoin _ =
      mempty

    doFriendAdd (_, EvtFriendRequest addr _) =
      Commands [CmdFriendAddNorequest addr]
    doFriendAdd _ =
      mempty
 
