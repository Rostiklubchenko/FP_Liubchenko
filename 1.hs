-- №1

applyRanges :: (a -> a -> a) -> [a] -> [(Int, Int)] -> [a]
applyRanges op arr ranges =
    [ foldl1 op (take (r - l + 1) (drop l arr)) | (l, r) <- ranges ]

main :: IO ()
main = do
    let arr = [1, 9, 7, 8, 3]
    let ranges = [(1, 4), (0, 2), (2, 4)]
    let op = (+) :: Int -> Int -> Int
    let result = applyRanges op arr ranges
    print result
