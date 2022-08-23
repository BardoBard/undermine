module Binary.Basic where
import Basic

isBin32 :: [Char] -> Bool
isBin32 bin = length bin > 32

toBin32 :: [Char] -> [Char]
toBin32 bin = if isBin32 bin then drop (length bin - 32) bin else bin

toBinx :: Int -> [Char] -> [Char]
toBinx n bin = drop (naturalPos (length bin - n)) bin

matchBin32 :: [Char] -> [Char]
matchBin32 x = replicate (32 - length (toBin32 x)) '0' ++ x

matchLength :: [Char] -> [Char] -> [Char]
matchLength x y = replicate (naturalPos (length x - length y)) '0' ++ y

biggestBin :: [Char] -> [Char] -> Bool
biggestBin [] _ = True
biggestBin _ [] = False
biggestBin (x:xs) (y:ys) 
                | x /= y    = x == '1' 
                | otherwise = biggestBin xs ys

biggestBinMatched :: [Char] -> [Char] -> [Char]
biggestBinMatched x y 
                | biggestBin x' y' = x'
                | otherwise               = y'
              where 
                  x' = matchLength y x
                  y' = matchLength x y


smallestBinMatched x y
                | biggestBin x' y' = y'
                | otherwise        = x'
              where 
                  x' = matchLength y x
                  y' = matchLength x y