module Binary.Operators where

import Binary.Logicgates
import Binary.Basic

fullDifference :: Char -> Char -> Char -> Char
fullDifference x y = xorB $ xorB x y

borrow :: Char -> Char -> Char -> (Char -> Char) -> Char
borrow x y bin f = orB (andB (f (xorB x y)) bin) (andB (f x) y)

borrowBin :: [Char] -> [Char] -> (Char -> Char) -> [Char]
borrowBin x y = borrowBin' (matchLength y x) (matchLength x y) '0'

borrowBin' :: [Char] -> [Char] -> Char -> (Char -> Char) -> [Char]
borrowBin' _  []  _  _  = []
borrowBin' []  _  _  _  = []
borrowBin' x y b f = borrowBin' (init x)  (init y) (borrow (last x) (last y) b f) f ++ [borrow (last x) (last y) b f]

--test :: [Char] -> [Char] -> [Char]

print' :: Char -> Char -> Char -> [Char]
print' x y z = x : y : z : ['-']

binMin :: [Char] -> [Char] -> [Char]
binMin x y = reverse $ zipWith3 fullDifference (reverse $ biggestBinMatched x y) (reverse $ smallestBinMatched x y) (reverse (borrowBin x y minB ++ "0"))

binPlus :: [Char] -> [Char] -> [Char]
binPlus x y = reverse $ zipWith3 fullDifference (reverse ('0' : matchLength x y) ) (reverse ('0' : matchLength y x)) (reverse (borrowBin x y plusB ++ "0"))