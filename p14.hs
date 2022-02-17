module Main where

import Data.List ( maximumBy )
import Data.Ord  ( comparing )
import qualified Data.MemoCombinators as Memo


collatz :: Int -> Int
collatz = Memo.integral collatz'
    where
        collatz' 1 = 1
        collatz' n | even n    = 1 + collatz (n `div` 2)
                   | otherwise = 2 + collatz (((3*n)+1) `div` 2)


answer :: Int
answer = fst . maximumBy (comparing snd) $ map (\x -> (x, collatz x)) [1..999999]


main :: IO ()
main =
    print answer
