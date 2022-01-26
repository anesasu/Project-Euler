module Main where

import Data.List ( find )


dividesAll :: Int -> Bool
dividesAll n = sum (map (n `mod`) [3..19]) == 0


answer :: Maybe Int
answer = find dividesAll [20,40..]


main :: IO ()
main = do
    print answer
