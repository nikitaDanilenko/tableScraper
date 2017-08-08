module TableScraper where

import Data.Text                ( Text, pack, unpack, replace, splitOn )
import Data.Text.IO             ( writeFile, readFile )
import qualified Data.Text as T ( unlines, intercalate, lines, concat )
import Text.XML.HXT.Core
import Text.Printf              ( printf )
import Data.Function            ( on )
import Data.List                ( groupBy )

import Prelude hiding           ( writeFile, readFile )

data Table = Table { headerLine :: [Text], cells :: [[Text]] }

toCSV :: Table -> Text
toCSV = toCSVWithSeparator (pack ",")

toCSVWithSeparator :: Text -> Table -> Text
toCSVWithSeparator sep table =
  T.unlines (map (T.intercalate sep) (headerLine table : cells table))

instance Show Table where
  show = unpack . toCSV

-- Collect table subtrees from the overall tree.
tables :: ArrowXml a => a XmlTree XmlTree
tables = deep (hasName "table")

-- Collect the subtrees of specific headers (thead -> tr -> th).
headers :: ArrowXml a => a XmlTree String
headers = hasName "thead" /> hasName "tr" /> hasName "th" /> getText

-- Collect the body subtree of the HTML document.
body :: ArrowXml a => a XmlTree XmlTree
body = getChildren >>> hasName "tbody"

-- Collect a single row subtree.
rows :: ArrowXml a => a XmlTree XmlTree
rows = getChildren >>> hasName "tr"

-- Collect the columns subtrees.
columns :: ArrowXml a => a XmlTree String
columns = getChildren >>> hasName "td" /> getText

-- Collect all headers of all tables. Each header line is listes in its own list.
fetchHeaders :: ArrowXml a => a XmlTree [String]
fetchHeaders = listA (getChildren >>> headers)

-- Collect all lines of a document.
fetchLines :: ArrowXml a => a XmlTree [[String]]
fetchLines = listA (body >>> rows >>> listA columns)

deepPack :: [[String]] -> [[Text]]
deepPack = map (map pack)

{-
  Fetch the text at a given location,
  smooth out the document (umlauts and possibly offending "meta" tag,
  which may occur),
  and extract the tables represented in the given document.
-}
fetchTables :: String -> IO [Table]
fetchTables fileName = do 
  text <- readFile fileName
  let html = unpack (fixOffending (removeThird text))
      doc  = readString [withErrors yes, withWarnings no] html
      ts   = doc >>> tables
  hs <- runX (ts >>> fetchHeaders)
  rrs <- runX (ts >>> fetchLines)
  return (zipWith Table (deepPack hs) (map deepPack rrs))

{- Replace all possibly offending elements with proper HTML representations.
   The implementation is not particularly efficient,
   but convenient.
-}
fixOffending :: Text -> Text
fixOffending = foldr ((.) . uncurry (replace `on` pack)) id pairs where
  pairs = [("</br>", "/"), ("ö", "&ouml;"), ("ä", "&auml"), ("ü", "&uuml;"),
           ("Ö", "&Ouml;"), ("Ä", "&Auml;"), ("Ü", "&Uuml;"), ("ß", "&szlig;"),
           ("<th></th>", "<th> </th>"), ("<br>", "/")]

-- Remove the third line of a document.
removeThird :: Text -> Text
removeThird = T.unlines . uncurry (\xs ys -> concat [xs, ys]) . (take 2 &&& drop 3) . T.lines

{- Remove the last occurring dot and everything following that dot in a text.
   This has some interesting effects for strings like "...",
   but is a practical heuristic to the concept of a file extension removal. -}
stripExtension :: Text -> Text
stripExtension text = T.intercalate dot (init (splitOn dot text)) where
  dot = pack "."

leadingZeroes :: Bool -> Int -> Int -> String
leadingZeroes fill fillTo num = if fill then '-' : printf ("%0" ++ show fillTo ++ "d") num else ""

{- 
Given a file name and a list of tables,
write out the tables to properly named documents.
-}
writeTables :: String -> [Table] -> IO ()
writeTables prefix tables = do
  let pieces       = map toCSV tables
      tnum         = length tables
      fill         = tnum > 1
      fillTo       = length (show tnum)
      prePrefix    = unpack (stripExtension (pack prefix))

      {- The files are exported as -}
      mkFileName :: Int -> String
      mkFileName i = concat [prePrefix, leadingZeroes fill fillTo i, ".csv"]
  mapM_ (\(i, text) -> writeFile (mkFileName i)  text) (zip [1 .. ] pieces) 