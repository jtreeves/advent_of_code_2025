module GetInput where

import System.IO

-- Input reading utilities for Haskell solutions

-- Read input from input.txt for the given day
getInput :: Int -> IO [String]
getInput day = do
    let path = getInputPath day
    readInput path

-- Read input from test_N.txt for the given day and test number
getTestInput :: Int -> Int -> IO [String]
getTestInput day testNum = do
    let path = getTestInputPath day testNum
    readInput path

-- Return the path to input.txt for the given day
getInputPath :: Int -> FilePath
getInputPath day = "../data/input.txt"

-- Return the path to test_N.txt for the given day and test number
getTestInputPath :: Int -> Int -> FilePath
getTestInputPath day testNum = "../data/test_" ++ show testNum ++ ".txt"

-- Read input file and return lines as a list
readInput :: FilePath -> IO [String]
readInput filePath = do
    content <- readFile filePath
    return $ lines $ trim content

-- Read input file and return raw content
readInputRaw :: FilePath -> IO String
readInputRaw filePath = readFile filePath

-- Helper function to trim whitespace
trim :: String -> String
trim = f . f
    where f = reverse . dropWhile (== ' ')
