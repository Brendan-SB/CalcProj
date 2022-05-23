module Bread
  ( finder
  , formatter
  , Bread
  ) where

import Control.Monad
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Char
import Graphics.PDF (PDF)
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
  mapM findBread urls

filterURLS :: [Tag String] -> [Tag String]
filterURLS tags = do
  let tag = TagOpen "h2" [("class", "nm-post-title")]
  filter (~== TagOpen "a" [("href", "")]) $
    filterItems 0 tags tag (takeWhile (~/= TagClose "h2"))

findBread :: Tag String -> IO Bread
findBread tag = do
  let url = fromAttrib "href" tag
  page <- openURL url
  let tags = parseTags page
  return $ formatBread tags

formatBread :: [Tag String] -> Bread
formatBread tags = do
  let (title, author) = filterInfo tags
  let ingredients = filterIngredients tags
  let instructions = filterInstructions tags
  Bread
    { title = cleanString $ fromTagText title
    , author = cleanString $ fromTagText author
    , ingredients = map (\t -> cleanString $ fromTagText t) ingredients
    , instructions = map (\t -> cleanString $ fromTagText t) instructions
    }

filterInfo :: [Tag String] -> (Tag String, Tag String)
filterInfo tags = do
  let section =
        takeWhile (~/= TagClose "header") $
        dropWhile
          (~/= TagOpen "header" [("class", "nm-post-header entry-header")])
          tags
  let title =
        head $
        filter (~== TagText "") $
        takeWhile (~/= TagClose "h1") $ dropWhile (~/= TagOpen "h1" []) section
  let author =
        head $
        filter (~== TagText "") $
        takeWhile (~/= TagClose "a") $ dropWhile (~/= TagOpen "a" []) section
  (title, author)

filterIngredients :: [Tag String] -> [Tag String]
filterIngredients tags = do
  let tag =
        TagOpen
          "ul"
          [ ( "class"
            , "zrdn-list zrdn-ingredients-list bullets zrdn-element_ingredients")
          ]
  filter (~== TagText "") $
    filterItems 0 tags tag (takeWhile (~/= TagClose "div"))

filterInstructions :: [Tag String] -> [Tag String]
filterInstructions tags = do
  let tag =
        TagOpen "ul" [("class", "zrdn-list zrdn-instructions-list nobullets")]
  let items = filterItems 0 tags tag (takeWhile (~/= TagClose "ul"))
  filter
    (~== TagText "")
    (if length items == 0
       then takeWhile (~/= TagClose "ul") $
            dropWhile
              (~/= TagOpen
                     "ul"
                     [ ( "class"
                       , "zrdn-list zrdn-instructions-list nobullets  zrdn-element_instructions")
                     ])
              tags
       else items)

filterItems ::
     Int
  -> [Tag String]
  -> Tag String
  -> ([Tag String] -> [Tag String])
  -> [Tag String]
filterItems i tags tag f =
  if i >= (length tags)
    then []
    else if (tags !! i) ~== tag
           then do
             let items = (f $ drop i tags)
             items ++ filterItems (i + 1 + length items) tags tag f
           else filterItems (i + 1) tags tag f

cleanString :: String -> String
cleanString s = unwords $ words s
