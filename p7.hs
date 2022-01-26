module Main where


isPrime :: Int -> Bool
isPrime n | n < 2          = False
          | n == 2         = True
          | even n         = False
          | n == 3         = True
          | n `mod` 3 == 0 = False
          | otherwise      = 0 `notElem` map (n `mod`) [5,7..(floor . sqrt $ fromIntegral n)]


primes :: [Int]
primes = 2 : filter isPrime [3,5..]


answer :: Int
answer = primes!!10000


main :: IO ()
main = do
    print answer
