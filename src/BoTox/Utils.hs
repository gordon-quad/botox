{-# LANGUAGE Arrows, FlexibleContexts #-}
module BoTox.Utils where

import BoTox.Types

import Control.Arrow
import Control.Auto hiding (many,option)
import Control.Auto.Blip
import Control.Monad.Reader
import Data.Either.Extra
import Data.List
import Data.Maybe
import Network.Tox.C
import Prelude hiding ((.), id)
import Text.Parsec

--
-- Parsers
---

-- A parser that modifies the argument parser to accept whitespace after it
spaced :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
spaced = (<* spaces)

-- Take everything
leftOvers :: Stream s m Char => ParsecT s u m String
leftOvers = many anyToken

-- Bool parser
bool :: Stream s m Char => ParsecT s u m Bool
bool = choice [try true, try false]

true :: Stream s m Char => ParsecT s u m Bool
true = True <$ (choice $ map (try . string) ["True", "true", "On", "on", "1", "Yes", "yes"])

false :: Stream s m Char => ParsecT s u m Bool
false = False <$ (choice $ map (try . string) ["False", "false", "Off", "off", "0", "No", "no"])

-- Integer parser
integer :: Stream s m Char => ParsecT s u m Int
integer = read <$> many1 digit
  
--
-- Command processing
--
parseCmd :: Monad m => [String] -> ParsecT String () Identity a  -> Auto m (d, String) (Blip (d, Either ParseError a))
parseCmd cmds parseArgs =  emitJusts processInput >>> modifyBlips parseCommand
  where
    processInput :: (d, String) -> Maybe (d, String)
    processInput (x, s) = eitherToMaybe $ parse (((,) x) <$> (choice (map (try . string) cmds) *> choice [try $ skipMany1 space, eof] *> leftOvers)) "CmdParser" s
    parseCommand (x, s) = (x, parse (parseArgs <* eof) "CmdParser" s)

parseGroupEventCmd :: Monad m => [String] -> ParsecT String () Identity a -> Auto m Event (Blip (Conference, Either ParseError a))
parseGroupEventCmd cmdPrefixes parseArgs =
  arr snd >>> emitJusts filterGroupMsgs >>> perBlip (parseCmd cmdPrefixes parseArgs) >>> joinB
  where
    filterGroupMsgs (EvtConferenceMessage conf _ MessageTypeNormal msg) = Just (conf, msg)
    filterGroupMsgs _                                                   = Nothing

parseFriendEventCmd :: Monad m => [String] -> ParsecT String () Identity a -> Auto m Event (Blip (Friend, Either ParseError a))
parseFriendEventCmd cmdPrefixes parseArgs =
  arr snd >>> emitJusts filterFriendMsgs >>> perBlip (parseCmd cmdPrefixes parseArgs) >>> joinB
  where
    filterFriendMsgs (EvtFriendMessage friend MessageTypeNormal msg) = Just (friend, msg)
    filterFriendMsgs _                                               = Nothing

parseGroupBotCmd :: Monad m => [String] -> ParsecT String () Identity a -> Auto m GroupEvent (Blip (Either ParseError a))
parseGroupBotCmd cmdPrefixes parseArgs = proc (_time, (EvtGroupMessage _ msg)) -> do
  modifyBlips snd . parseCmd cmdPrefixes parseArgs -< ((), msg)


--
-- Events filtering
--
masterEvents :: Auto m Event (Blip Event)
masterEvents = emitOn checkIsMaster
  where
    checkIsMaster (_, EvtFriendName f _)             = friendIsMaster f
    checkIsMaster (_, EvtFriendStatusMessage f _)    = friendIsMaster f
    checkIsMaster (_, EvtFriendStatus f _)           = friendIsMaster f
    checkIsMaster (_, EvtFriendConnectionStatus f _) = friendIsMaster f
    checkIsMaster (_, EvtFriendMessage f _ _)        = friendIsMaster f
    checkIsMaster (_, EvtConferenceInvite f _ _)     = friendIsMaster f
    checkIsMaster _                                  = False

---
--- Auxillary
---
groupBotSay :: String -> GroupCommands
groupBotSay s = GroupCommands [CmdGroupMessage MessageTypeNormal s]

botFriendSay :: Friend -> String -> Commands
botFriendSay f s = Commands [CmdFriendSendMessage (friendNum f) MessageTypeNormal s]

botConferenceSay :: Conference -> String -> Commands
botConferenceSay c s = Commands [CmdConferenceSendMessage (conferenceNum c) MessageTypeNormal s]
