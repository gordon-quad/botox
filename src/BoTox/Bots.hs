module BoTox.Bots where

import BoTox.Types

import Control.Auto.Serialize
import BoTox.Bots.GroupBot
import BoTox.Bots.TitleBot
import BoTox.Bots.RollBot

bots :: MonadTB m => [ToxBot m]
bots = [ serializing' "groups.db" groupBot
       , perGroup titleBot
       , perGroup rollBot ]
