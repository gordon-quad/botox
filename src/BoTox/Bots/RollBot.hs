{-# LANGUAGE Arrows, FlexibleContexts #-}
module BoTox.Bots.RollBot where

import BoTox.Types
import BoTox.Utils
import BoTox.Utils.DiceParser

import Control.Auto hiding (many)
import Control.Auto.Effects
import Control.Monad
import Control.Monad.IO.Class
import Data.Function (on)
import Data.List
import Prelude hiding ((.), id)
import System.Random (randomRIO, newStdGen)
import Text.Parsec

rollBot :: MonadIO m => GroupBot m
rollBot = proc event -> do
  infaCmds <- parseGroupBotCmd ["%инфа", "%info"] infaArgsParser -< event
  doCmds <- parseGroupBotCmd ["%зделоть", "%делать"] doArgsParser -< event
  diceCmds <- parseGroupBotCmd ["%dice", "%d", "%к"] diceArgsParser -< event
  outInfaCmds <- fromBlips mempty . arrMB (liftIO . doInfa) -< infaCmds
  outDoCmds <- fromBlips mempty . arrMB (liftIO . doDo) -< doCmds
  outDiceCmds <- fromBlips mempty . arrMB (liftIO . doDice) -< diceCmds
  id -< mconcat [outInfaCmds, outDoCmds, outDiceCmds]
  where
    infaArgsParser = leftOvers
    diceArgsParser = diceParser `sepBy` (char ',' *> spaces)
    doArgsParser = (many (noneOf ",")) `sepBy` (char ',' *> spaces)

    doDo (Right strs) = do
      vals <- liftIO $ forM strs $ \x -> (randomRIO (0,1000) :: IO Integer) >>= return . (,) x
      let v = sortVs vals
      let s = sum . snd . unzip $ v
      let outstr = intercalate "\n" $ map (\(ss,x) -> ss ++ " - " ++ show (x * 100 `div` s) ++ "%") v
      return $ groupBotSay outstr
    doDo (Left err) = return . groupBotSay . show $ err

    sortVs :: [(String, Integer)] -> [(String, Integer)]
    sortVs = sortBy (flip compare `on` snd)

    doInfa (Right str) = do
      rinfa <- randomRIO (0,146) :: IO Integer
      return $ groupBotSay $ str ++ " - инфа " ++ show rinfa ++ "%"
    doInfa (Left err) = return . groupBotSay . show $ err

    doDice (Right dice) = do
      diceStrs <- mapM getDiceStr dice
      return $ groupBotSay $ intercalate "\n" diceStrs
    doDice (Left err) = return . groupBotSay . show $ err

    getDiceStr diceExpr = do
      gen <- newStdGen
      let (s, _) = evalDiceStr diceExpr gen
      let (val, _) = evalDice diceExpr gen
      return $ s ++ " = " ++ show val
