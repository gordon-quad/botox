{-# LANGUAGE Arrows #-}
module BoTox.Bots.RollBot where

import BoTox.Bots.Utils
import BoTox.Types
import Control.Auto
import Control.Auto.Effects
import Control.Monad.IO.Class
import Data.List
import Control.Monad
import Data.Function (on)
import Data.String.Utils (strip, split)
import Network.Tox.C
import Prelude hiding ((.), id)
import System.Random (randomRIO)

rollBot :: MonadIO m => GroupBot m
rollBot = proc event -> do
  infaCmds <- parseGroupBotCmd "%инфа" parseInfaArgs -< event
  doCmds <- parseGroupBotCmd "%зделоть" parseDoArgs -< event
  outInfaCmds <- fromBlips (GroupCommands []) . arrMB (liftIO . doInfa) -< infaCmds
  outDoCmds <- fromBlips (GroupCommands []) . arrMB (liftIO . doDo) -< doCmds
  id -< mconcat [outInfaCmds, outDoCmds]
  where
    parseInfaArgs [] = Nothing
    parseInfaArgs smth = Just (unwords smth)

    parseDoArgs [] = Nothing
    parseDoArgs smth = Just (map strip $ split "," (unwords smth))

    doDo strs = do
      vals <- liftIO $ forM strs $ \x -> (randomRIO (0,1000) :: IO Integer) >>= return . (,) x
      let v = sortVs vals
      let s = sum . snd . unzip $ v
      let outstr = intercalate "\n" $ map (\(ss,x) -> ss ++ " - " ++ show (x * 100 `div` s) ++ "%") v
      return $ GroupCommands [CmdGroupMessage MessageTypeNormal outstr]

    sortVs :: [(String, Integer)] -> [(String, Integer)]
    sortVs = sortBy (flip compare `on` snd)

    doInfa str = do
      rinfa <- randomRIO (0,146) :: IO Integer
      let outstr = str ++ " - инфа " ++ show rinfa ++ "%"
      return $ GroupCommands [CmdGroupMessage MessageTypeNormal outstr]
