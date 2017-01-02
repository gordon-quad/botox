module Data.BoTox.Config where

import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BS
import           Data.String
import           Data.Word

toxBootstrapAddress :: String
toxBootstrapAddress = ""

toxBootstrapKey :: BS.ByteString
toxBootstrapKey = fst . Base16.decode . fromString $
                  "F404ABAA1C99A9D37D61AB54898F56793E1DEF8BD46B1038B9D822E8460FAB67"

toxBootstrapPort :: Word16
toxBootstrapPort = 1234


toxSavedataFilename :: String
toxSavedataFilename = "botox.tox"


masterKeys :: [BS.ByteString]
masterKeys = map (fst . Base16.decode . fromString) $
             [ "040F75B5C8995F9525F9A8692A6C355286BBD3CF248C984560733421274F0365",
               "FFE3B2073F02285C159FB1D338A247853A725FC74573951C7EF8BE4898671C57" ]

isMaster :: BS.ByteString -> Bool
isMaster key = elem key masterKeys
  
botName :: String
botName = "Нургл"
