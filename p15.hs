module Main where

import qualified Data.MemoCombinators as Memo


paths :: (Int, Int) -> Int
paths = Memo.pair Memo.integral Memo.integral paths'
    where
        paths' :: (Int,Int) -> Int
        paths' (0,_) = 1
        paths' (_,0) = 1
        paths' (x,y) | x == y    = paths (x-1,y) * 2
                     | otherwise = paths (x-1,y) + paths (x,y-1)


answer :: Int
answer = paths (20,20)


main :: IO ()
main =
    print answer
