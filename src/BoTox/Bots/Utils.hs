{-# LANGUAGE Arrows #-}
module BoTox.Bots.Utils where

import           BoTox.Types
import qualified Data.BoTox.Config as Cfg

import           Control.Arrow
import           Control.Auto
import           Control.Auto.Blip
import           Control.Monad.Reader
import           Data.Maybe
import           Data.List
import           Data.String.Utils
import           Network.Tox.C
import           Prelude hiding ((.), id)

parseCmd :: [String] -> (String -> a) -> Auto m (d, String) (Blip (d, a))
parseCmd cmds parseArgs = emitJusts (getRequest . args)
  where
    getRequest (x, (a:_)) = Just $ (x, parseArgs (strip a))
    getRequest (_, []) = Nothing
    args (x, str) = (x, mapMaybe ((flip stripPrefix) str) cmds)

parseGroupEventCmd :: Monad m => [String] -> (String -> a) -> Auto m Event (Blip (Conference, a))
parseGroupEventCmd cmdPrefixes parseArgs =
  arr snd >>> emitJusts filterGroupMsgs >>> perBlip (parseCmd cmdPrefixes parseArgs) >>> joinB
  where
    filterGroupMsgs (EvtConferenceMessage conf _ MessageTypeNormal msg) = Just (conf, msg)
    filterGroupMsgs _                                                   = Nothing

parseFriendEventCmd :: Monad m => [String] -> (String -> a) -> Auto m Event (Blip (Friend, a))
parseFriendEventCmd cmdPrefixes parseArgs =
  arr snd >>> emitJusts filterFriendMsgs >>> perBlip (parseCmd cmdPrefixes parseArgs) >>> joinB
  where
    filterFriendMsgs (EvtFriendMessage friend MessageTypeNormal msg) = Just (friend, msg)
    filterFriendMsgs _                                               = Nothing

parseGroupBotCmd :: Monad m => [String] -> (String -> b) -> Auto m GroupEvent (Blip b)
parseGroupBotCmd cmdPrefixes parseArgs = proc (_time, (EvtGroupMessage _ msg)) -> do
  modifyBlips snd . parseCmd cmdPrefixes parseArgs -< ((), msg)


masterEvents :: MonadTB m => Auto m Event (Blip Event)
masterEvents = arrM checkIsMaster >>> onJusts
  where
    checkIsMaster evt@(_, EvtFriendName f _)             = checkMasterFriend f evt
    checkIsMaster evt@(_, EvtFriendStatusMessage f _)    = checkMasterFriend f evt
    checkIsMaster evt@(_, EvtFriendStatus f _)           = checkMasterFriend f evt
    checkIsMaster evt@(_, EvtFriendConnectionStatus f _) = checkMasterFriend f evt
    checkIsMaster evt@(_, EvtFriendMessage f _ _)        = checkMasterFriend f evt
    checkIsMaster evt@(_, EvtConferenceInvite f _ _)     = checkMasterFriend f evt
    checkIsMaster _                                      = return Nothing

    checkMasterFriend :: MonadTB m => Friend -> Event -> m (Maybe Event)
    checkMasterFriend (Friend fn) evt = do
      tox <- getTox
      pk <- liftIO $ toxFriendGetPublicKey tox (fromIntegral fn)
      case pk of
        Left _ -> return Nothing
        Right key -> return $ if Cfg.isMaster key
                              then (Just evt)
                              else Nothing

groupBotSay :: String -> GroupCommands
groupBotSay s = GroupCommands [CmdGroupMessage MessageTypeNormal s]

botFriendSay :: Friend -> String -> Commands
botFriendSay f s = Commands [CmdFriendSendMessage f MessageTypeNormal s]

botConferenceSay :: Conference -> String -> Commands
botConferenceSay c s = Commands [CmdConferenceSendMessage c MessageTypeNormal s]
