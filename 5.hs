import System.IO (hFlush, stdout)
-- №5
ask :: Int -> IO Bool
ask x = do
    putStr $ "Задумане число менше, ніж " ++ show x ++ "? (Yes/No): "
    hFlush stdout
    ans <- getLine
    return (ans `elem` ["Yes", "yes", "y", "Y"])

guess :: Int -> Int -> IO ()
guess low high
    | low == high = putStrLn $ "Ваше число: " ++ show (low - 1)
    | otherwise   = do
        let mid = (low + high) `div` 2
        smaller <- ask mid
        if smaller
            then guess low mid
            else guess (mid + 1) high

main :: IO ()
main = do
    putStrLn "Задумайте число від 0 до 99."
    guess 0 99