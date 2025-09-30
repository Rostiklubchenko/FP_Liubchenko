-- №3
import Data.IORef

makeCounter :: IO (IO Int)
makeCounter = do
    ref <- newIORef 0
    return $ do
        modifyIORef ref (+1)
        readIORef ref

main :: IO ()
main = do
    counter <- makeCounter
    a <- counter
    print a
    b <- counter
    print b
    c <- counter
    print c


-- Alternative implementation without IORef --

-- makeCounter :: Int -> (Int, Int -> (Int, Int -> (Int, Int)))
-- makeCounter n = (n, \_ -> makeCounter (n+1))

-- next :: (Int, Int -> (Int, Int -> (Int, Int))) -> (Int, (Int, Int -> (Int, Int -> (Int, Int))))
-- next (val, f) = (val, f 0)

-- main :: IO ()
-- main = do
--     let c0 = makeCounter 1
--     let (v1, c1) = next c0
--     print v1   -- 1
--     let (v2, c2) = next c1
--     print v2   -- 2
--     let (v3, c3) = next c2
--     print v3   -- 3