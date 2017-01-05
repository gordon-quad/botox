{-# LANGUAGE Arrows #-}

module BoTox where

import           BoTox.Commands
import           BoTox.Types
import           BoTox.Bots (bots)
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

runTox :: Options -> ToxBot ToxBotApp -> IO ()
runTox opts bot = must $ withOptions opts $
  \optsPtr ->
    must $ withTox optsPtr $
    \tox -> withCHandler tox $ do
      addr <- toxSelfGetAddress tox
      putStrLn $ BS.unpack $ Base16.encode addr
      _ <- toxBootstrap tox Cfg.toxBootstrapAddress Cfg.toxBootstrapPort Cfg.toxBootstrapKey
      _ <- toxSelfSetName tox Cfg.botName
      savedata <- toxGetSavedata tox
      BS.writeFile Cfg.toxSavedataFilename savedata
      runToxBotApp (runBot bot) (ToxBotRState tox)

runBot :: MonadTB m => ToxBot m -> m ()
runBot bot = do
  time <- liftIO epochTime
  tox <- getTox
  (Commands cmds, bot') <- stepAuto bot (time, EvtStartup)
  liftIO $ mapM_ (runCommand tox) cmds
  loop bot'
                                         

loop :: MonadTB m => ToxBot m -> m ()
loop bot = do
  tox <- getTox
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
              

chatBot :: MonadTB m => ToxBot m
chatBot = mconcat bots
  
                     
runBoTox :: IO ()
runBoTox = do
  savedata <- readToxSavedata Cfg.toxSavedataFilename
  runTox (toxOptions savedata) chatBot
