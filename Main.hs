module Main where

import System.Environment ( getArgs )

import TableScraper       ( fetchTables, writeTables )


main :: IO ()
main = getArgs >>= mapM_ (\f -> fetchTables f >>= writeTables f)