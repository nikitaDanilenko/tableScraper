module TableScraper where

import Data.Text                ( Text, pack, unpack, replace )
import Data.Text.IO             ( writeFile, readFile )
import qualified Data.Text as T ( unlines, intercalate )
import Text.XML.HXT.Core
import Text.Printf              ( printf )
import Data.Function            ( on )
import Data.List                ( groupBy )
import System.Environment       ( getArgs )

import Prelude hiding           ( writeFile, readFile )

data Table = Table { headerLine :: [Text], cells :: [[Text]] }

toCSV :: Table -> Text
toCSV = toCSVWithSeparator (pack ",")

toCSVWithSeparator :: Text -> Table -> Text
toCSVWithSeparator sep table =
  T.unlines (map (T.intercalate sep) (headerLine table : cells table))

instance Show Table where
  show = unpack . toCSV

tables :: ArrowXml a => a XmlTree XmlTree
tables = deep (hasName "table")

headers :: ArrowXml a => a XmlTree String
headers = hasName "thead" /> hasName "tr" /> hasName "th" /> getText

body :: ArrowXml a => a XmlTree XmlTree
body = getChildren >>> hasName "tbody"

rows :: ArrowXml a => a XmlTree XmlTree
rows = getChildren >>> hasName "tr"

columns :: ArrowXml a => a XmlTree String
columns = getChildren >>> hasName "td" /> getText

fetchHeaders :: ArrowXml a => a XmlTree [String]
fetchHeaders = listA (getChildren >>> headers)

fetchLines :: ArrowXml a => a XmlTree [[String]]
fetchLines = listA (body >>> rows >>> listA columns)

deepPack :: [[String]] -> [[Text]]
deepPack = map (map pack)

fetchTables :: String -> IO [Table]
fetchTables fileName = do 
  text <- readFile fileName
  let html = unpack (fixOffending text)
      doc  = readString [withErrors yes, withWarnings no] html
      ts   = doc >>> tables
  hs <- runX (ts >>> fetchHeaders)
  rrs <- runX (ts >>> fetchLines)
  return (zipWith Table (deepPack hs) (map deepPack rrs))

fixOffending :: Text -> Text
fixOffending = foldr ((.) . uncurry (replace `on` pack)) id pairs where
  pairs = [("</br>", "/"), ("ö", "&ouml;"), ("ä", "&auml"), ("ü", "&uuml;"),
           ("Ö", "&Ouml;"), ("Ä", "&Auml;"), ("Ü", "&Uuml;"), ("ß", "&szlig;"),
           ("<th></th>", "<th> </th>")]

writeTables :: String -> [Table] -> IO ()
writeTables prefix tables = do
  let pieces       = map toCSV tables
      fillTo       = length (show (length tables))
      mkFileName :: Int -> String
      mkFileName i = concat [prefix, "-", printf ("%0" ++ show fillTo ++ "d") i, ".csv"]
  mapM_ (\(i, text) -> writeFile (mkFileName i)  text) (zip [1 .. ] pieces) 

main :: IO ()
main = getArgs >>= mapM_ (\f -> fetchTables f >>= writeTables f)