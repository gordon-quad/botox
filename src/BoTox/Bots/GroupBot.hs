{-# LANGUAGE Arrows #-}
module BoTox.Bots.GroupBot where

import           BoTox.Types
import           BoTox.Utils

import           Control.Auto
import           Control.Auto.Blip
import           Control.Monad.Reader
import qualified Data.ByteString as BS
import           Data.Map as M
import           Network.Tox.C
import           Prelude hiding ((.), id)
import           Text.Parsec

groupBot :: MonadTB m => ToxBot m
groupBot = proc event@(_time, evt) -> do
  autoInviteMsgs <- perBlip resolvePk . parseFriendEventCmd ["autoinvite"] autoInviteArgsParser -< event
  onlineFriends <- perBlip resolvePk . emitJusts friendOnlineEvent -< evt
  inviteMsgs <- parseFriendEventCmd ["invite"] inviteArgsParser -< event
  masterInviteConfs <- filterB isInviteEvent . masterEvents -< event
  friendRequests <- emitOn isFriendRequestEvent -< event

  autoInvites <- updateAutoInvites -< autoInviteMsgs

  let queryAutoInvites :: ((Friend, BS.ByteString), a) -> ((Friend, Bool), a)
      queryAutoInvites ((friend, pk), x) = ((friend, M.findWithDefault False pk autoInvites), x)
  
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

    upd m ((_friend, key), Right (Just val)) = M.insert key val m
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
        Commands [CmdConferenceInvite friend (Conference 0)]
    doFriendOnline ((_, False), _) =
        mempty
 
    doInvite (friend, Right confNum) = Commands [CmdConferenceInvite friend (Conference confNum)]
    doInvite (friend, Left _)      = botFriendSay friend "Usage: invite [conference]"

    doMasterJoin (_, EvtConferenceInvite friend ConferenceTypeText cookie) =
      Commands [CmdConferenceJoin friend cookie]
    doMasterJoin _ =
      mempty
  
    doFriendAdd (_, EvtFriendRequest addr _) =
      Commands [CmdFriendAddNorequest addr]
    doFriendAdd _ =
      mempty
    
                                
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
