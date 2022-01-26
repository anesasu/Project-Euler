module Main where


fib :: Int -> Int
fib 1 = 1
fib 2 = 2
fib n = fib (n-1) + fib (n-2)


answer :: Int
answer = sum (filter even fibs)
    where
        fibs = takeWhile (<= 4000000) (map fib [1..])


main :: IO ()
main = do
    print answer
