module Main where


triNum :: Int -> Int
triNum 1 = 1
triNum n = n + triNum (n-1)


triNums :: [Int]
triNums = map triNum [1..]


flatten :: [[a]] -> [a]
flatten a = [y | x <- a, y <- x]


factors :: Int -> [Int]
factors n = flatten [[x, n `div` x] | x <- [1..(floor $ sqrt $ fromIntegral n)], n `mod` x == 0]


answer :: Int
answer = head (dropWhile (\x -> length x < 500) (map factors triNums)) !! 1


main :: IO ()
main =
    print answer
