module Bread
    ( finder,
      Bread
    ) where

import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Data.Char
import Text.HTML.TagSoup

data Bread = Bread { title :: String, author:: String, content :: String }

url :: String
url = "https://breadtopia.com/category/recipes/"

finder :: IO String
finder = do
  putStrLn "Bread is being found..."

  initReq <- parseRequest url

  let req = initReq {
    method = C8.pack "GET"
  }

  man <- newManager tlsManagerSettings 
  response <- httpLbs req man

  let body = L8.unpack $ responseBody response

  putStrLn "Bread has been found."

  return body
