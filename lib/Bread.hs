module Bread where

import qualified Control.Monad.Parallel as P
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Maybe
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import System.Directory
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
  return $ L8.unpack $ responseBody response

finder :: IO String
finder = do
  putStrLn "Downloading main page"
  openURL breadUrl

formatter :: String -> IO [Bread]
formatter input = do
  let tags =
        filterURLS $
        takeWhile (~/= TagClose "ul") $
        dropWhile (~/= TagOpen "ul" [("id", "nm-blog-list")]) (parseTags input)
  bread <- P.mapM findAndWrite tags
  return $ catMaybes bread

filterURLS :: [Tag String] -> [Tag String]
filterURLS tags = do
  let tag = TagOpen "h2" [("class", "nm-post-title")]
  filter (~== TagOpen "a" [("href", "")]) $
    filterItems 0 tags tag (takeWhile (~/= TagClose "h2"))

findBread :: Tag String -> IO Bread
findBread tag = do
  let url = fromAttrib "href" tag
  putStrLn $ "Downloading " ++ url
  page <- openURL url
  return $ formatBread $ parseTags page

formatBread :: [Tag String] -> Bread
formatBread tags = do
  let (ti, a) = filterInfo tags
  let ing = filterIngredients tags
  let ins = filterInstructions tags
  Bread
    { title = cleanString $ fromTagText ti
    , author = cleanString $ fromTagText a
    , ingredients = filter (/= "") $ map (\t -> cleanString $ fromTagText t) ing
    , instructions =
        filter (/= "") $ map (\t -> cleanString $ fromTagText t) ins
    }

filterInfo :: [Tag String] -> (Tag String, Tag String)
filterInfo tags = do
  let section =
        takeWhile (~/= TagClose "header") $
        dropWhile
          (~/= TagOpen "header" [("class", "nm-post-header entry-header")])
          tags
  let t =
        head $
        filter (~== TagText "") $
        takeWhile (~/= TagClose "h1") $ dropWhile (~/= TagOpen "h1" []) section
  let a =
        head $
        filter (~== TagText "") $
        takeWhile (~/= TagClose "a") $ dropWhile (~/= TagOpen "a" []) section
  (t, a)

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

breadToMD :: Bread -> String
breadToMD bread =
  "# " ++
  title bread ++
  "\n" ++
  "## By: " ++
  author bread ++
  "\n\n" ++
  (concat $ map (\i -> "* " ++ i ++ "\n") $ ingredients bread) ++
  "\n" ++
  (concat $
   map (\(n, i) -> (show ((n + 1) :: Integer)) ++ ". " ++ i ++ "\n") $
   zip [0 ..] $ instructions bread)

findAndWrite :: Tag String -> IO (Maybe Bread)
findAndWrite tag = do
  bread <- findBread tag
  let md = breadToMD bread
  if (length md) > 0
    then do
      let p = ("recipes/" ++ (title bread) ++ ".md")
      putStrLn $ "Formatting to markdown and writing to " ++ p
      createDirectoryIfMissing False "recipes"
      writeFile p md
      return $ Just bread
    else do
      return Nothing
