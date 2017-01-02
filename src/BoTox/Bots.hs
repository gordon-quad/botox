module BoTox.Bots where

import BoTox.Types

import Control.Auto.Serialize
import BoTox.Bots.GroupBot
import BoTox.Bots.TitleBot

bots :: [ToxBot (ToxBotApp t)]
bots = [ serializing' "groups.db" groupBot
       , perGroup titleBot ]
