module Main where


divides :: Int -> Bool
divides n | n `mod` 3 == 0 = True
          | n `mod` 5 == 0 = True
          | otherwise      = False


answer :: Int
answer = sum (filter divides [1,2..999])


main :: IO ()
main = do
    print answer
