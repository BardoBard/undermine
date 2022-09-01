module Modes.Shared where

import Data.List ( elemIndex ) 
import System.Console.ANSI

--my libs

import Basic ( replaceAtIndex, iterateFunc, badImul', naturalPos )
import Binary.Shift ( shiftBinL32, shiftBinR32 )
import Binary.Logicgates ( xorB' )
import Binary.Conversion ( toDec, decToBase )


--this works every time
range :: Int -> Int -> Int -> Int
range min max seed = nextUInt seed `mod` (max - min + 1) + min

nextUInt seed =
    toDec (xorB' x' y')
    where
        seedBin = decToBase 2 seed ""
        x     = xorB' seedBin $ shiftBinL32 11 seedBin --add if '1' then two else continue
        x'    = xorB' x       $ shiftBinR32 8 x
        y     = decToBase 2   (iterateFunc  3 badImul' seed) ""
        y'    = xorB' y       $ shiftBinR32 19 y

reduceWeight :: [Int] -> Int -> Int
reduceWeight [] _ = 0
reduceWeight (x:xs) y
                | (y - x) <= 0 = 0
                | otherwise    = 1 + reduceWeight xs (y - x)

sum' x y z
            | x == (-1) = 0
            | otherwise = z !! naturalPos y


getIndex seed items sum = reduceWeight items $ range 1 sum seed

replaceAtIndex' :: (Eq a1, Num a1, Num a2) => a1 -> Int -> [a2] -> [a2]
replaceAtIndex' x x2 list
    | x == (-1) = list
    | otherwise = replaceAtIndex x2 0 list

findIndex' :: String -> String -> [String] -> IO Int
findIndex' name msg list = do
    if null name
        then
            return (-1)
        else do
            let y = elemIndex name list
            case y of
                Just x -> return x
                Nothing -> do
                    print "ERROR: Wrong input"
                    print msg
                    x <- getLine :: IO String
                    findIndex' x msg list

findIndexWithError :: String -> String -> [String] -> IO Int
findIndexWithError name msg list = do
    let y = elemIndex name list
    case y of
        Just x -> return x
        Nothing -> do
            print "ERROR: Wrong input"
            print msg
            x <- getLine :: IO String
            findIndexWithError x msg list


nextRoom :: String -> [String] -> IO Int
nextRoom room list = do
    clearScreen

    putStr "Pick one of the following: "
    putStr "\n\n"
    print list
    putStr "\n"

    print $ room ++ ": "
    x <- getLine :: IO String

    findIndex' x (room ++ ": ") list