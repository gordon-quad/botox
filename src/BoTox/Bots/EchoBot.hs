{-# LANGUAGE Arrows, FlexibleContexts #-}
module BoTox.Bots.EchoBot where

import BoTox.Types
import BoTox.Utils

import Control.Auto
import Control.Auto.Blip
import Control.Monad
import Data.List
import Prelude hiding ((.), id)

echoBot :: Monad m => GroupBot m
echoBot = proc event -> do
  pingCmds <- parseGroupBotCmd ["%ping", "%пинг", "%dbjl", "%gbyu"] leftOvers -< event
  outPongs <- fromBlips mempty . modifyBlips doPong -< pingCmds
  id -< outPongs
  where
    doPong (Right str) = groupBotSay $ "PONG " ++ str
    doPong (Left _)  = groupBotSay "Как ты смог меня сломать?"
