module Main where


answer :: Int
answer = abs $ (sum [x*x | x <- [1..100]]) - (sum [1..100] ^ 2)


main :: IO ()
main = do
    print answer
