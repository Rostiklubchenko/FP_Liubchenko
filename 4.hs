-- №4
prod5 :: Int -> Int -> Int -> Int -> Int -> Int
prod5 a b c d e = a * b * c * d * e

prod5c :: Int -> Int -> Int -> Int -> Int -> Int
prod5c a = \b -> \c -> \d -> \e -> a * b * c * d * e

lab4 :: IO ()
lab4 = do
    print (prod5 1 2 3 4 5)
    let f1 = prod5 2
    print (f1 3 4 5 6)
    let f2 = prod5 2 3
    print (f2 4 5 6)
    let f3 = prod5 2 3 4
    print (f3 5 6)
    let f4 = prod5 2 3 4 5
    print (f4 6)

lab4c :: IO ()
lab4c = do
    print (prod5c 1 2 3 4 5)
    print ((prod5c 4) 5 6 7 8)
    print ((prod5c 3 4 5) 6 7)
    print ((prod5c 2 3 4 5) 6)
