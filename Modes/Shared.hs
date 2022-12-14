module Modes.Shared ( rooms, showDisplay, getIndex, replaceAt', sum', dibbleItem ) where

--my libs

import Basic ( replaceAt, iterateFunc, badImul', naturalPos )
import Binary.Shift ( shiftBinL32, shiftBinR32 )
import Binary.Logicgates ( xorB' )
import Binary.Conversion ( toDec, decToBase )

rooms = ["Mine 1", "Mine 2", "Mine 3", "Mine 4", "Dungeon 1", "Dungeon 2", "Dungeon 3", "Dungeon 4", "Halls 1", "Halls 2", "Halls 3", "Halls 4", "Caverns 1", "Caverns 2", "Caverns 3", "Caverns 4", "Core 1", "Core 2", "Core 3", "Core 4"]


--this works every time
range :: Int -> Int -> Int -> Int
range min max seed = nextUInt seed `mod` (max - min + 1) + min

nextUInt seed =
    toDec (xorB' x' y')
    where
        seedBin = decToBase 2 seed ""
        x     = xorB' seedBin $ shiftBinL32 11 seedBin
        x'    = xorB' x       $ shiftBinR32 8 x
        y     = decToBase 2   (iterateFunc  3 badImul' seed) "" --TODO: make a binary multiplication function
        y'    = xorB' y       $ shiftBinR32 19 y

reduceWeight :: [Int] -> Int -> Int
reduceWeight [] _ = 0
reduceWeight (x:xs) y
                | (y - x) <= 0 = 0
                | otherwise    = 1 + reduceWeight xs (y - x)

sum' x y z
            | x == (-1) = 0
            | otherwise = z !! naturalPos y


showDisplay list = zipWith (\ x y -> if y == (-1) then "Nothing" else list !! y) list

dibbleItem seed items sum = if doesDibbleHaveItem then getIndex seed items sum else (-1)
    where
        doesDibbleHaveItem = range 1 8 seed == 8

    
getIndex seed items sum = reduceWeight items $ range 1 sum seed

replaceAt' :: (Eq a1, Num a1, Num a2) => a1 -> Int -> [a2] -> [a2]
replaceAt' x x2 list
    | x == (-1) = list
    | otherwise = replaceAt x2 0 list