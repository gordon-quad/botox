{-# LANGUAGE OverloadedStrings, Arrows #-}
module BoTox.Bots.TitleBot where

import           BoTox.Types
import           BoTox.Utils

import           Control.Auto
import           Control.Monad
import           Control.Auto.Blip
import           Control.Auto.Effects (arrMB)
import           Control.Exception (try)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Codec.Text.IConv as IConv
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.ByteString as BS
import qualified Data.List as L
import           Data.Maybe
import qualified Data.String as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Conduit.Binary as CB
import           Data.Conduit
import           Network.HTTP.Simple
import           Network.HTTP.Types.Header
import Prelude hiding           ((.), id)
import           Text.HTML.TagSoup
import           Text.HTML.TagSoup.Match
import           Text.Regex


titleBot :: MonadIO m => GroupBot m
titleBot = proc (_time, EvtGroupMessage _ msg) -> do

  blipMsg <- emitJusts containsUrl -< msg

  result <- arrMB (liftIO . getTitle) -< blipMsg

  cmds <- fromBlips mempty . modifyBlips groupBotSay -< result

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
    Right (contentType, webpage) -> do
      let errorText    = "Webpage has no title."
          parsed       =  parseTags $ webpage
          titleTags    =  takeWhile (not . tagCloseNameLit "title") $ dropWhile (not . tagOpenNameLit "title") parsed
          result       =  mconcat $ catMaybes $ map maybeTagText titleTags
      case result of 
        "" ->  return errorText
        r -> return $ mapError . TE.decodeUtf8' . LB.toStrict $ toCharset (decodeCharset contentType) r -- To do: handle web pages that don't use UTF8
    Left ex -> return $ T.pack $ "Fetching page title failed due to " ++ ex
  where
    mapError = either (S.fromString . show) id


getWebPage :: String -- | Url
            -> IO (Either String (Maybe BS.ByteString, LB.ByteString)) -- | Either "HttpError" Webpage
getWebPage url = do
  eResponse <- try (getUrl url) :: IO (Either HttpException (Maybe BS.ByteString, LB.ByteString))
  case eResponse of
    Right (contentType, response) -> return $ Right $ (contentType, response)
    Left ex -> return $ Left $ "HTTP Exception: " ++ show ex

getUrl :: String -> IO (Maybe BS.ByteString, LB.ByteString)
getUrl url = do
  req <- parseRequest url
  withResponse req (\resp -> liftM ((,) (listToMaybe $ getResponseHeader hContentType resp)) $ (getResponseBody resp $$ CB.take 10240))

decodeCharset :: Maybe BS.ByteString -> String
decodeCharset Nothing = "UTF-8"
decodeCharset (Just c) = (either (\_ -> "UTF-8") (matchCharset . T.unpack) $ TE.decodeUtf8' c)

matchCharset :: String -> String
matchCharset t = maybe "UTF-8" head $ matchRegex (mkRegex "charset=([a-zA-Z0-9-]+)") t

toCharset :: String -> LB.ByteString -> LB.ByteString
toCharset enc title = IConv.convert enc "UTF-8" title

