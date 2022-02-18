module Main where

import Data.Char ( digitToInt )


asWords :: Int -> [Char]
asWords n | n <= 0    = ""
          | n <= 9    = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] !! (n-1)
          | n <= 19   = ["ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"] !! (n-10)
          | n <= 99   = ["twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"] !! (floor (fromIntegral (n `div` 10)) - 2) ++ " " ++ asWords (read (tail (show n)))
          | n == 1000 = "one thousand"
          | otherwise = asWords (digitToInt (head (show n))) ++ " hundred" ++ (if res == "" then "" else " and " ++ res)
    where
        res = asWords (read (tail (show n)))



removeSpaces :: [Char] -> [Char]
removeSpaces []     = ""
removeSpaces (x:xs) | x == ' '  = removeSpaces xs
                    | otherwise = x:removeSpaces xs


answer :: Int
answer = sum $ map (length . removeSpaces . asWords) [1..1000]


main :: IO ()
main =
    print answer
