module Main where

import Data.Char ( digitToInt )


answer :: Int
answer = sum $ [digitToInt x | x <- show (2^1000)]


main :: IO ()
main =
    print answer
