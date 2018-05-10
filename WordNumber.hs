-- Solution for Haskell Programming from first principles
-- Chapter 8 Numbers into Words

module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n =
    case n of
        1 -> "one"
        2 -> "two"
        3 -> "three"
        4 -> "four"
        5 -> "five"
        6 -> "six"
        7 -> "seven"
        8 -> "eight"
        9 -> "nine"
        otherwise -> "not a number"


digits :: Int -> [Int]
digits n 
    | n < 10 = [n `mod` 10]
    | otherwise = digits (div n 10) ++ digits (mod n 10)

wordNumber :: Int -> String
-- wordNumber n = reverse $ tail $ reverse $ concat $ map (++ "-") $ map digitToWord $ digits n
wordNumber n = concat $ intersperse "-" $ map digitToWord $ digits n