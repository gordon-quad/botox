{-# LANGUAGE Arrows #-}
module BoTox.Bots.Utils where

import           BoTox.Types
import qualified Data.BoTox.Config as Cfg

import           Control.Auto
import           Control.Auto.Blip
import           Control.Monad.Reader
import           Network.Tox.C
import           Prelude hiding ((.), id)

parseCmd :: Monad m => String -> ([String] -> Maybe b) -> Auto m (a, String) (Blip (a, b))
parseCmd cmd parseArgs = emitJusts (getRequest cmd parseArgs . (\(x, y) -> (x, words y)))

getRequest :: String -> ([String] -> Maybe b) -> (a, [String]) -> Maybe (a, b)
getRequest cmdPrefix parseArgs (x, (cmd : args)) 
  | cmdPrefix == cmd = do
      parsed <- parseArgs args
      return (x, parsed)
  | otherwise        = Nothing
getRequest _ _ _     = Nothing

parseGroupEventCmd :: Monad m => String -> ([String] -> Maybe b) -> Auto m Event (Blip (Conference, b))
parseGroupEventCmd cmd parseArgs = proc (_time, evt) -> do
  cmds <- joinB . perBlip (parseCmd cmd parseArgs) . emitJusts filterGroupMsgs -< evt
  id -< cmds
  where
    filterGroupMsgs (EvtConferenceMessage conf _ MessageTypeNormal msg) = Just (conf, msg)
    filterGroupMsgs _                                                   = Nothing

parseFriendEventCmd :: Monad m => String -> ([String] -> Maybe b) -> Auto m Event (Blip (Friend, b))
parseFriendEventCmd cmd parseArgs = proc (_time, evt) -> do
  cmds <- joinB . perBlip (parseCmd cmd parseArgs) . emitJusts filterGroupMsgs -< evt
  id -< cmds
  where
    filterGroupMsgs (EvtFriendMessage friend MessageTypeNormal msg) = Just (friend, msg)
    filterGroupMsgs _                                               = Nothing

parseGroupBotCmd :: Monad m => String -> ([String] -> Maybe b) -> Auto m GroupEvent (Blip b)
parseGroupBotCmd cmd parseArgs = proc (_time, (EvtGroupMessage _ msg)) -> do
  cmds <- modifyBlips snd . parseCmd cmd parseArgs -< ((), msg)
  id -< cmds
  
masterEvents :: Auto (ToxBotApp t) Event (Blip Event)
masterEvents = proc event -> do
  outEvents <- onJusts . arrM checkIsMaster -< event
  id -< outEvents
  where
    checkIsMaster evt@(_, EvtFriendName f _)             = checkMasterFriend f evt
    checkIsMaster evt@(_, EvtFriendStatusMessage f _)    = checkMasterFriend f evt
    checkIsMaster evt@(_, EvtFriendStatus f _)           = checkMasterFriend f evt
    checkIsMaster evt@(_, EvtFriendConnectionStatus f _) = checkMasterFriend f evt
    checkIsMaster evt@(_, EvtFriendMessage f _ _)        = checkMasterFriend f evt
    checkIsMaster evt@(_, EvtConferenceInvite f _ _)     = checkMasterFriend f evt
    checkIsMaster _                                      = return Nothing

    checkMasterFriend :: Friend -> Event -> ToxBotApp t (Maybe Event)
    checkMasterFriend (Friend fn) evt = do
      tox <- ask
      pk <- liftIO $ toxFriendGetPublicKey tox (fromIntegral fn)
      case pk of
        Left _ -> return Nothing
        Right key -> return $ if Cfg.isMaster key
                              then (Just evt)
                              else Nothing
