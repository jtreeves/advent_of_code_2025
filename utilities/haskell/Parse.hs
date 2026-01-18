module Parse where

import Data.Char (isDigit)

-- Parsing utilities for Haskell solutions

-- Parse integers from a line of text
parseInts :: String -> [Int]
parseInts line = map read $ words line
