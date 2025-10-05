-- №6

-- Y-комбінатор
y :: (a -> a) -> a
y f = f (y f)

----------------------------------------------------


factY :: (Integer -> Integer) -> (Integer -> Integer) -- Нерекурсивна
factY f n = if n == 0 then 1 else n * f (n - 1)


fact :: Integer -> Integer -- Рекурсивна
fact = y factY

----------------------------------------------------


fibY :: (Integer -> Integer) -> (Integer -> Integer) -- Нерекурсивна 
fibY f n
    | n == 0    = 0
    | n == 1    = 1
    | otherwise = f (n - 1) + f (n - 2)

fib :: Integer -> Integer -- Рекурсивна
fib = y fibY

----------------------------------------------------

main :: IO ()
main = do
    putStrLn "Factorial:"
    print (fact 0)
    print (fact 5)
    print (fact 7)

    putStrLn "\nFibonacci:"
    print (fib 0)
    print (fib 1)
    print (fib 5)
    print (fib 10)
