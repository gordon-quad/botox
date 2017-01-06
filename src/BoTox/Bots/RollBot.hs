{-# LANGUAGE Arrows #-}
module BoTox.Bots.RollBot where

import BoTox.Bots.Utils
import BoTox.Types
import BoTox.Utils.DiceParser

import Control.Auto
import Control.Auto.Effects
import Control.Monad
import Control.Monad.IO.Class
import Data.Function (on)
import Data.List
import Data.String.Utils (split)
import Prelude hiding ((.), id)
import System.Random (randomRIO, newStdGen)

rollBot :: MonadIO m => GroupBot m
rollBot = proc event -> do
  infaCmds <- parseGroupBotCmd ["%инфа", "%info"] id -< event
  doCmds <- parseGroupBotCmd ["%зделоть","%делать"] parseDoArgs -< event
  diceCmds <- parseGroupBotCmd ["%d","%dice","%к"] id -< event
  outInfaCmds <- fromBlips (GroupCommands []) . arrMB (liftIO . doInfa) -< infaCmds
  outDoCmds <- fromBlips (GroupCommands []) . arrMB (liftIO . doDo) -< doCmds
  outDiceCmds <- fromBlips (GroupCommands []) . arrMB (liftIO . doDice) -< diceCmds
  id -< mconcat [outInfaCmds, outDoCmds, outDiceCmds]
  where
    parseDoArgs smth = split "," smth

    doDo strs = do
      vals <- liftIO $ forM strs $ \x -> (randomRIO (0,1000) :: IO Integer) >>= return . (,) x
      let v = sortVs vals
      let s = sum . snd . unzip $ v
      let outstr = intercalate "\n" $ map (\(ss,x) -> ss ++ " - " ++ show (x * 100 `div` s) ++ "%") v
      return $ groupBotSay outstr

    sortVs :: [(String, Integer)] -> [(String, Integer)]
    sortVs = sortBy (flip compare `on` snd)

    doInfa str = do
      rinfa <- randomRIO (0,146) :: IO Integer
      return $ groupBotSay $ str ++ " - инфа " ++ show rinfa ++ "%"

    doDice str =
      case parseDice str of
        Left err -> return $ groupBotSay (show err)
        Right expr -> do
          gen <- newStdGen
          let (s, _) = evalDiceStr expr gen
          let (val, _) = evalDice expr gen
          return $ groupBotSay (s ++ " = " ++ show val)
