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


primeFactors :: Int -> [Int]
primeFactors n = primeFactors' n (filter (\x -> n `mod` x == 0) primes)


primeFactors' :: Int -> [Int] -> [Int]
primeFactors' n p | n == p0         = [n]
                  | n `mod` p0 == 0 = p0 : primeFactors' (n `div` p0) p
                  | otherwise       = primeFactors' n ps
    where
        p0 = head p
        ps = tail p


answer :: Int
answer = maximum $ primeFactors 600851475143


main :: IO ()
main = do
    print answer
