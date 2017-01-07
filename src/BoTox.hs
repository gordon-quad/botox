{-# LANGUAGE Arrows #-}

module BoTox where

import           BoTox.Commands
import           BoTox.Types
import           BoTox.Bots (bots)

import           Control.Auto.Run
import           Control.Concurrent
import           Control.Exception
import           Control.Monad.Loops
import           Control.Monad.Reader
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BS
import           Data.Maybe
import           Network.Tox.C
import           Prelude hiding ((.), id)
import           System.Log.Formatter
import           System.Log.Handler hiding (setLevel)
import           System.Log.Handler.Simple
import           System.Log.Logger
import           System.Random

comp :: String
comp = "BoTox"

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

runTox :: ToxBot ToxBotApp -> IO ()
runTox bot = do
  maybeCfg <- readConfig
  let cfg = fromMaybe (error "Cannot read config") maybeCfg
  logFileHandler <- fileHandler (logFile cfg) (logLevel cfg)
  let logFileHandler' = setFormatter logFileHandler (simpleLogFormatter $ logFormat cfg)
  updateGlobalLogger comp (setLevel $ logLevel cfg)
  updateGlobalLogger comp (setHandlers [logFileHandler'])
  infoM comp "Bot is starting up"
  savedata <- readToxSavedata (toxSavedataFilename cfg)
  must $ withOptions (toxOptions savedata) $ \optsPtr ->
    must $ withTox optsPtr $
    \tox -> withCHandler tox $ do
      bootstrapNode <- liftM (bootstrapNodes cfg !! ) $ randomRIO (0, (length $ bootstrapNodes cfg) - 1)
      addr <- toxSelfGetAddress tox
      infoM comp (("Bot address is " ++) $ BS.unpack $ Base16.encode addr)
      _ <- toxBootstrap tox (bootstrapAddress bootstrapNode) (fromIntegral $ bootstrapPort bootstrapNode) (bootstrapPublicKey bootstrapNode)
      _ <- toxSelfSetName tox (botName cfg)
      sd <- toxGetSavedata tox
      BS.writeFile (toxSavedataFilename cfg) sd
      runToxBotApp (iterateM_ runBot bot) (ToxBotRState tox cfg)


runBot :: MonadTB m => ToxBot m -> m (ToxBot m)
runBot bot = do
  tox <- getTox
  cfg <- getConfig
  ToxData events _ <- liftIO $ stepTox tox cfg
  (cmds, bot') <- overList bot events
  let Commands cmds' = mconcat cmds
  liftIO $ mapM_ (runCommand tox cfg) cmds'
  return bot'
             

stepTox :: Tox ToxData -> Config -> IO ToxData
stepTox tox cfg = do
  td <- newMVar $ ToxData [] cfg
  toxIterate tox td
  interval <- toxIterationInterval tox
  threadDelay $ fromIntegral $ interval * 10000
  takeMVar td
              

chatBot :: MonadTB m => ToxBot m
chatBot = mconcat bots
  
                     
runBoTox :: IO ()
runBoTox = do
  runTox chatBot
