module Bread
  ( finder
  , formatter
  , Bread
  ) where

import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Char
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Text.HTML.TagSoup

data Bread =
  Bread
    { title :: String
    , author :: String
    , content :: String
    }
  deriving (Show)

breadUrl :: String
breadUrl = "https://breadtopia.com/category/recipes/"

openURL :: String -> IO String
openURL url = do
  initReq <- parseRequest url
  let req = initReq {method = C8.pack "GET"}
  man <- newManager tlsManagerSettings
  response <- httpLbs req man
  let body = L8.unpack $ responseBody response
  return body

finder :: IO String
finder = openURL breadUrl

formatter :: String -> IO (Maybe [Bread])
formatter input = do
  let urls =
        dropWhile (~/= TagOpen "a" [("href", "")]) $
        dropWhile (~/= TagOpen "h2" [("class", "nm-post-title")]) $
        dropWhile (~/= TagOpen "h2" [("class", "nm-post-title")]) $
        dropWhile (~/= TagOpen "ul" [("id", "nm-blog-list")]) (parseTags input)
  print $ innerText urls
  formatBread 0 urls

formatBread :: Int -> [Tag String] -> IO (Maybe [Bread])
formatBread i tags =
  if i == length tags
    then return Nothing
    else case (maybeTagText $ tags !! i) of
           Just url -> do
             page <- openURL url
             let content = parseTags page
             let bread = Bread {title = "", author = "", content = ""}
             nextBread <- formatBread (i + 1) tags
             case nextBread of
               Just nextBread -> return $ Just $ [bread] ++ nextBread
               Nothing -> return $ Just [bread]
           Nothing -> return Nothing
