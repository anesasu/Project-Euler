module Main where


answer :: Int
answer = maximum $ filter (\x -> show x == reverse (show x)) [x*y | x <- [100..999], y <- [100..999]]


main :: IO ()
main = do
    print answer
