module Utils where

import System.IO
import Data.Char (isDigit)

-- Common utility functions for Haskell solutions
readInput :: FilePath -> IO [String]
readInput filePath = do
    content <- readFile filePath
    return $ lines $ trim content

readInputRaw :: FilePath -> IO String
readInputRaw filePath = readFile filePath

parseInts :: String -> [Int]
parseInts line = map read $ words line
    where isDigit' c = isDigit c || c == '-'

trim :: String -> String
trim = f . f
    where f = reverse . dropWhile (== ' ')

-- Placeholder for additional common utilities
