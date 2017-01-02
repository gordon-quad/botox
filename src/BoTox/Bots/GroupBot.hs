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

groupBot :: ToxBot (ToxBotApp t)
groupBot = proc event@(_time, evt) -> do
  autoInviteMsgs <- perBlip resolvePk . parseFriendEventCmd "autoinvite" parseAutoInviteArgs -< event
  autoInvites <- updateAutoInvites -< autoInviteMsgs
  onlineFriends <- perBlip resolvePk . emitJusts friendOnlineEvent -< evt
  inviteMsgs <- parseFriendEventCmd "invite" parseInviteArgs -< event
  masterInviteConfs <- filterB isInviteEvent . masterEvents -< event

  let queryAutoInvites :: ((Friend, BS.ByteString), a) -> ((Friend, Bool), a)
      queryAutoInvites ((friend, pk), x) = ((friend, M.findWithDefault False pk autoInvites), x)
  
  aiCmds <- fromBlips (Commands []) . modifyBlips doAutoInviteCmds -< queryAutoInvites <$> autoInviteMsgs
  foCmds <- fromBlips (Commands []) . modifyBlips doFriendOnline -< queryAutoInvites <$> onlineFriends
  iCmds <- fromBlips (Commands []) . modifyBlips doInvite -< inviteMsgs
  micCmds <- fromBlips (Commands []) . modifyBlips doMasterJoin -< masterInviteConfs

  id -< mconcat [aiCmds, foCmds, iCmds, micCmds]
  where
    parseAutoInviteArgs ["on"]    = Just $ Just True
    parseAutoInviteArgs ["On"]    = Just $ Just True
    parseAutoInviteArgs ["yes"]   = Just $ Just True
    parseAutoInviteArgs ["Yes"]   = Just $ Just True
    parseAutoInviteArgs ["true"]  = Just $ Just True
    parseAutoInviteArgs ["True"]  = Just $ Just True
    parseAutoInviteArgs ["off"]    = Just $ Just False
    parseAutoInviteArgs ["Off"]    = Just $ Just False
    parseAutoInviteArgs ["no"]    = Just $ Just False
    parseAutoInviteArgs ["No"]    = Just $ Just False
    parseAutoInviteArgs ["false"] = Just $ Just False
    parseAutoInviteArgs ["False"] = Just $ Just False
    parseAutoInviteArgs []        = Just $ Nothing
    parseAutoInviteArgs _         = Nothing

    parseInviteArgs [a] = readMaybe a
    parseInviteArgs []  = Just 0
    parseInviteArgs _   = Nothing

    updateAutoInvites = scanB upd M.empty

    upd m ((_friend, key), Just val) = M.insert key val m
    upd m _                          = m

    friendOnlineEvent (EvtFriendConnectionStatus _ ConnectionNone) = Nothing
    friendOnlineEvent (EvtFriendConnectionStatus friend _)         = Just (friend, ())
    friendOnlineEvent _                                            = Nothing

    isInviteEvent (_, EvtConferenceInvite _ _ _) = True
    isInviteEvent _                              = False

    doAutoInviteCmds ((friend, _), Just True) =
      Commands [CmdFriendSendMessage friend MessageTypeNormal "Auto-invite turned on"]
    doAutoInviteCmds ((friend, _), Just False) =
      Commands [CmdFriendSendMessage friend MessageTypeNormal "Auto-invite turned off"]
    doAutoInviteCmds ((friend, autoInvite), Nothing) =
      Commands [CmdFriendSendMessage friend MessageTypeNormal ("Auto-invite is " ++ (show autoInvite)) ]

    doFriendOnline ((friend, True), _) =
        Commands [CmdConferenceInvite friend (Conference 0)]
    doFriendOnline ((_, False), _) =
        Commands []

    doInvite (friend, confNum) = Commands [CmdConferenceInvite friend (Conference confNum)]

    doMasterJoin (_, EvtConferenceInvite friend ConferenceTypeText cookie) =
      Commands [CmdConferenceJoin friend cookie]
    doMasterJoin _ =
      Commands []
    
                                
resolvePk :: Auto (ToxBotApp t) (Friend, a) ((Friend, BS.ByteString), a)
resolvePk = proc (friend, args) -> do
  pk <- arrM getPk -< friend
  id -< ((friend, pk), args)
  where
    getFriendNum (Friend num) = fromIntegral num

    getPk :: Friend -> ToxBotApp t (BS.ByteString)
    getPk friend = do
      tox <- ask
      pk <- liftIO $ toxFriendGetPublicKey tox (getFriendNum friend)
      case pk of
        Left _err -> return BS.empty
        Right key -> return key

