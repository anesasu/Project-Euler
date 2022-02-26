module Main where


day :: Int -> Int -> Int -> Int
day 1900 1 1 = 0
day    y m d | prev == 7 = 0
             | otherwise = prev
    where
        prev | d > 1               = day y     m     (d-1)  + 1
             | m == 1              = day (y-1) 12    31     + 1
             | m == 3              = day y     2     feb    + 1
             | m `elem` [4,6,9,11] = day y     (m-1) 31     + 1
             | otherwise           = day y     (m-1) 30     + 1

        feb | y `mod` 400 == 0 = 29
            | y `mod` 100 == 0 = 28
            | y `mod` 4   == 0 = 29
            | otherwise        = 28



dayName :: Int -> [Char]
dayName = (["Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"] !!)


days :: [Int]
days = [day y m 1 | y <- [1901..2000], m <- [1..12]]


answer :: Int
answer = length $ filter (== 6) days


main :: IO ()
main =
    print answer
