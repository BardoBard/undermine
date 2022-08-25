module Basic where

-- |
-- removes the last n elements from an array
--
-- e.g | pop 3 "10011"            -> "10"
--
-- e.g | pop 3 [1,3,5,6,7,8,9,10] -> [1,3,5,6,7]
--
-- returns: new array
pop :: Int -> [a] -> [a]
pop n list = take (length list - n) list



replace' :: [Int] -> [a] -> [a] -> [a]
replace' [] _ zs  = zs
replace' _ [] zs  = zs
replace' (x:xs) (y:ys) zs = replace' xs ys $ replaceAtIndex x y zs


replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex i x xs = take i xs ++ [x] ++ drop (i+1) xs

--this needs looking at
iterateFunc :: (Eq t1, Num t1) => t1 -> (t2 -> t2) -> t2 -> t2
iterateFunc 0 _ x = x
iterateFunc n f x = iterateFunc (n - 1) f (f x)

-- | 
-- used for undermine
--
-- e.g | badImul' 100 -> 181243325300
badImul' :: Int -> Int
badImul' = badImul 1812433253

-- | 
-- a bad way to do the javascript "imul"
--
-- e.g badImul 5 500 -> 2500
--
-- returns a multiplication of two Uint32
badImul :: Int -> Int -> Int
badImul x y = toUInt32 $ x * y

-- |
-- int to Uint32
--
-- returns: Uint32
toUInt32 :: Int -> Int
toUInt32 x = do
    let int32 = x `mod` 4294967295 - x `div` 4294967295 + 1
    if x > 4294967295
        then
            if int32 < 0
            then 4294967295 + int32 + 1
            else int32
        else x

-- |
-- int to Sint32
--
-- returns: Sint32
toInt32 :: Int -> Int
toInt32 x =
    if x > 2147483647
        then x - ceiling (fromIntegral x / 2147483647) * 2147483647
        else x

-- | 
-- can't go smaller than 0
--
-- e.g | nateralPos -10 -> 0
--
-- e.g | nateralPos 100 -> 100
--
-- returns: positive integer or 0
naturalPos :: Int -> Int
naturalPos x
    | x <= 0    = 0
    | otherwise = x