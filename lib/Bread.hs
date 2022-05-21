module Bread
  ( finder
  , formatter
  , Bread
  ) where

import Control.Monad
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Char
import Data.List
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

rmDups :: (Ord a) => [a] -> [a]
rmDups = map head . group . sort

formatter :: Int -> String -> IO [Bread]
formatter amount input = do
  let urls =
        rmDups $
        take (amount * 2) $
        filter (\t -> (fromAttrib "class" t) == "") $
        filter (~== TagOpen "a" [("href", "")]) $
        dropWhile (~/= TagOpen "h2" [("class", "nm-post-title")]) $
        dropWhile (~/= TagOpen "li" []) $
        dropWhile (~/= TagOpen "ul" [("id", "nm-blog-list")]) (parseTags input)
  print urls

  return []

  -- packBread 0 urls

packBread :: Int -> [Tag String] -> IO [Bread]
packBread i urls = do
  if i == length urls
    then return []
    else do
      bread <- formatBread $ urls !! i
      case bread of
        Just bread -> do
          nextBread <- (packBread (i + 1) urls)
          return $ ([bread] ++ nextBread)
        Nothing -> do
          nextBread <- (packBread (i + 1) urls)
          return nextBread

formatBread :: Tag String -> IO (Maybe Bread)
formatBread tag = do
  case tag of
    TagOpen _ _ -> do
      let url = fromAttrib "href" tag
      print url
      page <- openURL url
      let content = parseTags page
      let bread = Bread {title = "", author = "", content = ""}
      return $ Just bread
    _ -> return Nothing
