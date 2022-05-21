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
    , ingredients :: [String]
    , instructions :: [String]
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

formatter :: String -> IO [Bread]
formatter input = do
  let urls =
        filterURLS $
        takeWhile (~/= TagClose "ul") $
        dropWhile (~/= TagOpen "ul" [("id", "nm-blog-list")]) (parseTags input)
  packBread 0 urls

filterURLS :: [Tag String] -> [Tag String]
filterURLS tags = do
  let tag = TagOpen "h2" [("class", "nm-post-title")]
  let f =
        (\t ->
           filter (~== TagOpen "a" [("href", "")]) $
           takeWhile (~/= TagClose "h2") t)
  filterItems 0 tags tag f

packBread :: Int -> [Tag String] -> IO [Bread]
packBread i urls = do
  if i == length urls
    then return []
    else do
      let url = urls !! i
      bread <- findBread url
      nextBread <- (packBread (i + 1) urls)
      case bread of
        Just bread -> do
          return $ [bread] ++ nextBread
        Nothing -> do
          return nextBread

findBread :: Tag String -> IO (Maybe Bread)
findBread tag = do
  case tag of
    TagOpen _ _ -> do
      let url = fromAttrib "href" tag
      page <- openURL url
      let tags = parseTags page
      let bread = formatBread tags
      return $ Just bread
    _ -> return Nothing

formatBread :: [Tag String] -> Bread
formatBread tags = do
  let bread =
        Bread
          { title = ""
          , author = ""
          , instructions = []
          , ingredients = map fromTagText $ filterIngredients tags
          }
  bread

filterIngredients :: [Tag String] -> [Tag String]
filterIngredients tags = do
  let tag =
        TagOpen
          "ul"
          [ ( "class"
            , "zrdn-list zrdn-ingredients-list bullets zrdn-element_ingredients")
          ]
  let f = (\t -> filter (~== TagText "") $ takeWhile (~/= TagClose "div") t)
  filterItems 0 tags tag f

filterItems ::
     Int
  -> [Tag String]
  -> Tag String
  -> ([Tag String] -> [Tag String])
  -> [Tag String]
filterItems i tags tag f =
  if i >= length tags
    then []
    else do
      if (tags !! i) ~== tag
        then do
          let items = f $ drop i tags
          items ++ filterItems (i + 1 + length items) tags tag f
        else filterItems (i + 1) tags tag f
