module TableScraper where

import Text.HTML.Parser
import Data.Text         ( Text, pack, concatMap, unlines, intercalate )
import Data.Maybe        ( mapMaybe )

import Prelude hiding    ( concatMap, unlines )

isStartBy :: Text -> Token -> Bool
isStartBy tagName (TagOpen name _) = tagName == name
isStartBy _       _              = False

isEndBy :: Text -> Token -> Bool
isEndBy tagName (TagClose name) = tagName == name
isEndBy _       _               = False

openCloseBy :: Text -> (Token -> Bool, Token -> Bool)
openCloseBy tagName = (isStartBy tagName, isEndBy tagName)

tableName, headerName, rowName, cellName :: Text
tableName  = pack "table"
headerName = pack "th"
rowName    = pack "tr"
cellName   = pack "td"

isTableStart, isTableEnd :: Token -> Bool
(isTableStart, isTableEnd) = openCloseBy tableName

isHeaderStart, isHeaderEnd :: Token -> Bool
(isHeaderStart, isHeaderEnd) = openCloseBy headerName

isRowStart, isRowEnd :: Token -> Bool
(isRowStart, isRowEnd) = openCloseBy rowName

isCellStart, isCellEnd :: Token -> Bool
(isCellStart, isCellEnd) = openCloseBy cellName

toText :: Token -> Maybe Text
toText (ContentText text) = Just text
toText _                  = Nothing

data Table = Table { headerLine :: Maybe [Text], cells :: [[Text]] }

toCSV :: Table -> Text
toCSV = toCSVWithSeparator (pack ",")

toCSVWithSeparator :: Text -> Table -> Text
toCSVWithSeparator sep table =
  unlines (map (intercalate sep) (cells table))

parseTables :: [Token] -> [Table]
parseTables [] = [] 
parseTables ts = table : parseTables remainder where
  (table, remainder) = parseTable ts

takeChunksWith :: (a -> Bool) -> (a -> Bool) -> [a] -> ([a], [a])
takeChunksWith start end xs = 
  span (not . end) (drop 1 (dropWhile (not . start) xs))

--TODO Hier geht es weiter.
parseTable :: [Token] -> (Table, [Token])
parseTable ts = undefined
  --span (not . isTableEnd) (drop 1 (dropWhile (not . isTableStart) ts))

parseLine :: [Token] -> ([Text], [Token])
parseLine ts = (mapMaybe toText preParsed, drop 1 preRemainder) where
   (preParsed, preRemainder) = takeChunksWith isCellStart isCellEnd ts

--parseLines :: [Token] -> ([[Text]], )
