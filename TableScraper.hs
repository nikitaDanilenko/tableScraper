module TableScraper where

import Data.Text                ( Text, pack )
import Data.Text.IO             ( writeFile )
import qualified Data.Text as T ( unlines, intercalate )
import Text.XML.HXT.Core
import Text.Printf              ( printf )
import Data.Function            ( on )
import Data.List                ( groupBy )

import Prelude hiding           ( writeFile )

data Table = Table { headerLine :: [Text], cells :: [[Text]] }

toCSV :: Table -> Text
toCSV = toCSVWithSeparator (pack ",")

toCSVWithSeparator :: Text -> Table -> Text
toCSVWithSeparator sep table =
  T.unlines (map (T.intercalate sep) (cells table))

tables :: ArrowXml a => a XmlTree XmlTree
tables = deep (hasName "table")

headers :: ArrowXml a => a XmlTree String
headers = getChildren /> hasName "th" /> getText

rows :: ArrowXml a => a XmlTree XmlTree
rows = getChildren >>> hasName "tr"

columns :: ArrowXml a => a XmlTree String
columns = getChildren >>> hasName "td" /> getText

collectTable :: ArrowXml a => a XmlTree ([String], [String])
collectTable = tables >>> listA headers &&& (rows >>> listA columns)

deepPack :: [String] -> [Text]
deepPack = map pack

groupTables :: [([String], [String])] ->[Table]
groupTables = 
  map (\((headers, _) : rest) -> Table (deepPack headers) (map (deepPack . snd) rest)) . groupBy ((==) `on` fst)

fetchTables :: String -> IO [Table]
fetchTables fileName = 
  fmap groupTables (runX (readDocument [withErrors yes, withWarnings no] fileName >>> collectTable))

writeTables :: String -> [Table] -> IO ()
writeTables prefix tables = do
  let pieces       = map toCSV tables
      fillTo       = length (show (length tables))
      mkFileName :: Int -> String
      mkFileName i = concat [prefix, "-", printf ("%0" ++ show fillTo ++ "d") i]
  mapM_ (\(i, text) -> writeFile (mkFileName i)  text) (zip [1 .. ] pieces)    



-- main :: IO ()
-- main = do
--   args <- getArgs
