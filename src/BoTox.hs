{-# LANGUAGE Arrows #-}

module BoTox where

import           BoTox.Callbacks
import           BoTox.Commands
import           BoTox.Types
import qualified Data.BoTox.Config as Cfg

import           Prelude hiding ((.), id)
import           Control.Auto
import           Control.Auto.Run
import           Control.Exception
import           Network.Tox.C
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Base16 as Base16
import           Control.Concurrent.MVar
import           Control.Concurrent
import           Control.Monad.Reader
import           System.Posix.Time (epochTime)

toxOptions :: BS.ByteString -> Options
toxOptions savedata = Options
  { ipv6Enabled  = True
  , udpEnabled   = True
  , proxyType    = ProxyTypeNone
  , proxyHost    = ""
  , proxyPort    = 0
  , startPort    = 33445
  , endPort      = 33545
  , tcpPort      = 3128
  , savedataType = if savedata == BS.empty
                   then SavedataTypeNone
                   else SavedataTypeToxSave
  , savedataData = savedata
  }

getRight :: (Monad m, Show a) => Either a b -> m b
getRight (Left  l) = fail $ show l
getRight (Right r) = return r
    
      
must :: Show a => IO (Either a b) -> IO b
must = (getRight =<<)


readToxSavedata :: FilePath -> IO BS.ByteString
readToxSavedata f = catch (BS.readFile f) handler
  where
    handler :: IOException -> IO BS.ByteString
    handler _e = return BS.empty

runTox :: Options -> ToxBot (ToxBotApp ToxData) -> IO ()
runTox opts bot = must $ withOptions opts $
  \optsPtr ->
    must $ withTox optsPtr $
    \tox -> withCHandler tox $ do
      addr <- toxSelfGetAddress tox
      putStrLn $ BS.unpack $ Base16.encode addr
      _ <- toxBootstrap tox Cfg.toxBootstrapAddress Cfg.toxBootstrapPort Cfg.toxBootstrapKey
      runToxBotApp (runBot bot) tox

runBot :: ToxBot (ToxBotApp ToxData) -> ToxBotApp ToxData ()
runBot bot = do
  time <- liftIO epochTime
  tox <- ask
  (Commands cmds, bot') <- stepAuto bot (time, EvtStartup)
  liftIO $ mapM_ (runCommand tox) cmds
  loop bot'
                                         

loop :: ToxBot (ToxBotApp ToxData) -> ToxBotApp ToxData ()
loop bot = do
  tox <- ask
  ToxData events <- liftIO $ stepTox tox
  (cmds, bot') <- overList bot events
  let Commands cmds' = mconcat cmds
  liftIO $ mapM_ (runCommand tox) cmds'
  loop bot'
             

stepTox :: Tox ToxData -> IO ToxData
stepTox tox = do
  td <- newMVar $ ToxData []
  toxIterate tox td
  interval <- toxIterationInterval tox
  threadDelay $ fromIntegral $ interval * 10000
  takeMVar td
              
                     
runBoTox :: IO ()
runBoTox = do
  savedata <- readToxSavedata Cfg.toxSavedataFilename
  runTox (toxOptions savedata) echoBot


perGroup :: Monad m => GroupBot m -> ToxBot m
perGroup gb = proc evts -> do
  gevtBlips <- emitJusts confMsg -< evts
  cmdBlips <- perBlip (confMap gb) -< gevtBlips
  cmds <- fromBlips (Commands []) -< cmdBlips
  id -< cmds
  where
    confMsg (time, evt) = case evt of
                            EvtConferenceMessage cnf _ msgType msg -> 
                              Just (cnf, (time, EvtGroupMessage msgType msg))
                            _ -> Nothing

    toCmd conf (CmdGroupMessage msgType msg) = CmdConferenceSendMessage conf msgType msg

    confMap gbot = proc (conf, evt) -> do
      GroupCommands gcmds <- gbot -< evt
      let cmds = map (toCmd conf) gcmds
      id -< Commands cmds


echoBot :: Monad a => ToxBot a
echoBot = proc (_time, event) ->
  id -< Commands (doEvent event)
  where
    doEvent (EvtFriendRequest addr _msg) = [CmdFriendAddNorequest addr]
    doEvent (EvtFriendMessage friend msgType msg) = [CmdFriendSendMessage friend msgType msg] 
    doEvent _ = []