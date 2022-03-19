module Main where

import Data.Char ( digitToInt )


answer :: Int
answer = sum $ map digitToInt $ show $ product [2..100]


main :: IO ()
main =
    print answer
