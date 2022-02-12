module Main where

import Numeric ( showFFloat )


answer :: Int
answer = round $ head [a*b*(1000-(a+b)) | a <- [1..998], b <- [1..999-a], a^^2+b^^2==(1000-(a+b))^^2]


main :: IO ()
main = do
    print answer
