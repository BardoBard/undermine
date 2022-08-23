module Binary.Logicgates where 

import Binary.Basic
import Basic
import Data.Char
import Data.List

-- NOT

notB :: Char -> Char
notB x
        | x == '1'  = '0'
        | otherwise = '1'

-- | used for binary subtraction
--
-- same as notB
minB :: Char -> Char
minB = notB

--BUFFER

bufferB :: Char -> Char
bufferB x = x

-- | used for binary addition
--
-- same as a buffer
plusB :: Char -> Char
plusB = bufferB

-- ORs

orB :: Char -> Char -> Char
orB x y
        | x == '1' || y == '1' = '1'
        | otherwise            = '0'

norB :: Char -> Char -> Char
norB x y = notB $ orB x y

xorB :: Char -> Char -> Char
xorB x y
        | andB x y == '1'      = '0'
        | x == '1' || y == '1' = '1'
        | otherwise            = '0'

xorB' :: [Char] -> [Char] -> [Char]
xorB' x y = zipWith xorB (matchLength y x) (matchLength x y)

xnorB :: Char -> Char -> Char
xnorB x y = notB $ xorB x y

-- ANDs

andB :: Char -> Char -> Char
andB x y
        | x == '1' && y == '1' = '1'
        | otherwise            = '0'

nandB :: Char -> Char -> Char
nandB x y = notB $ andB x y

