-- №2
type Number = Int
type RealNum = Float
type Text = String

type ListOf a = [a]

type Pair a b = (a, b)

data MyList a = Empty | Cons a (MyList a)
    deriving Show

myLength :: MyList a -> Int
myLength Empty        = 0
myLength (Cons _ xs)  = 1 + myLength xs

toList :: MyList a -> [a]
toList Empty        = []
toList (Cons x xs)  = x : toList xs

main :: IO ()
main = do
    putStrLn "Type aliases"
    let x :: Number; x = 42
    let y :: RealNum; y = 3.14
    let name :: Text; name = "Rostyslav"
    let xs :: ListOf Number; xs = [1,2,3,4]
    let person :: Pair Text Number; person = (name, 20)

    putStrLn $ "Number: " ++ show x
    putStrLn $ "RealNum: " ++ show y
    putStrLn $ "Text: " ++ name
    putStrLn $ "ListOf Number: " ++ show xs
    putStrLn $ "Person: " ++ show person

    putStrLn "\nList data type"
    let lst = Cons 1 (Cons 2 (Cons 3 Empty))
    putStrLn $ "MyList length = " ++ show (myLength lst)
    putStrLn $ "MyList as normal list = " ++ show (toList lst)
