{-# LANGUAGE OverloadedStrings, Arrows #-}
module BoTox.Bots.TitleBot where

import           BoTox.Types

import           Control.Auto
import           Control.Auto.Blip
import           Control.Auto.Effects (arrMB)
import           Control.Exception (try)
import           Control.Lens
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.List as L
import qualified Data.String as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Network.HTTP.Client (defaultManagerSettings, managerResponseTimeout, HttpException(..))
import           Network.Tox.C
import           Network.Wreq
import Prelude hiding           ((.), id)
import           Text.HTML.TagSoup
import           Text.HTML.TagSoup.Match


titleBot :: MonadIO m => GroupBot m
titleBot = proc (_time, EvtGroupMessage _ msg) -> do

  blipMsg <- emitJusts containsUrl -< msg

  result <- arrMB (liftIO . getTitle) -< blipMsg

  cmds <- fromBlips (GroupCommands []) . modifyBlips (\x -> GroupCommands [CmdGroupMessage MessageTypeNormal x]) -< result

  id -< cmds

  where
    containsUrl :: Message -> Maybe Message
    containsUrl msg =
      if foldr (||) False $ map isStringUrl $ S.words msg
         then Just msg
         else Nothing

    isStringUrl :: String -> Bool
    isStringUrl str = "://" `L.isInfixOf` str
                   || "://" `L.isInfixOf` str

    getTitle :: Message -> IO Message
    getTitle msg = fmap ((">Title: " ++) . (L.intercalate "\n>Title: "))  $ fmap (map T.unpack) $ urlList $ S.words msg

    urlList :: [String] -> IO [T.Text]
    urlList [] = return []
    urlList (x:xs) =
      if isStringUrl x
         then do
           title' <- extractTitle x
           let title = if title' == "Notfound.org"
                          then "Error occured!"
                          else title'
           fmap (title :) (urlList xs)
         else urlList xs


extractTitle :: String -- | URL
             -> IO T.Text -- | Title
extractTitle url = do
  eitherWebPage <- getWebPage url  -- webpage :: L.ByteString
  case eitherWebPage of
    Right webpage -> do
      let errorText    = "Webpage has no title."
          parsed       =  parseTags webpage
          content      = getTagContent (S.fromString "title") null parsed
          maybeResult  = case fmap maybeTagText content of
                           (x:_) -> x
                           []    -> Nothing
      if  Prelude.foldr (||) False $ fmap (tagOpenNameLit "title") parsed
      then case maybeResult of
             Just result ->  return $ TE.decodeUtf8 $ LB.toStrict result -- To do: handle web pages that don't use UTF8
             Nothing     -> return "Could not fetch a title for that URL."
      else return errorText
    Left ex -> return $ T.pack $ "Fetching page title failed due to " ++ ex


getWebPage :: String -- | Url
            -> IO (Either String LB.ByteString) -- | Either "HttpError" Webpage
getWebPage url = do
  eResponse <- try (get url) :: IO (Either HttpException (Response LB.ByteString))
  case eResponse of
    Right response -> return $ Right $ response ^. responseBody
    Left ex -> return $ Left $ "HTTP Exception: " ++ show ex
