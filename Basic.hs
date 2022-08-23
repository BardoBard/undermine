module Basic where

pop :: Int -> [a] -> [a]
pop n list = take (length list - n) list

iterateFunc 0 _ x = x
iterateFunc n f x = iterateFunc (n - 1) f (f x)

badImul :: Int -> Int
badImul x = toUInt32 $ 1812433253 * x

toUInt32 :: Int -> Int
toUInt32 x = do
    let int32 = x `mod` 4294967295 - x `div` 4294967295 + 1
    if x > 4294967295
        then
            if int32 < 0
            then 4294967295 + int32 + 1
            else int32
        else x

toInt32 :: Int -> Int
toInt32 x =
    if x > 2147483647
        then x - ceiling (fromIntegral x / 2147483647) * 2147483647
        else x

naturalPos :: Int -> Int
naturalPos x 
    | x <= 0    = 0
    | otherwise = x