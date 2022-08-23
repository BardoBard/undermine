{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.Function

import System.IO

import Data.Char

import Data.List

import Data.Maybe

--my libs

import Basic

import Binary.Basic

import Binary.Shift

import Binary.Logicgates
import Binary.Operators
import Binary.Conversion



range :: Int -> Int -> Int -> Int
range min max seed = nextUInt seed `mod` (max - min + 1) + min



nextUInt seed = do
    let seedBin = decToBase 2 seed ""
    let x     = xorB' seedBin $ shiftBinL32 11 seedBin --add if '1' then two else continue
    let x'    = xorB' x       $ shiftBinR32 8 x
    let y     = decToBase 2   (iterateFunc  3 badImul' seed) ""
    let y'    = xorB' y       $ shiftBinR32 19 y
    toDec (xorB' x' y')

toTwos' :: [Char] -> [Char]
toTwos' bin
            | bin' == '1' = toTwos bin
            | otherwise   = bin
            where
                bin'      = head $ toBinx 32 bin

{-     y <- iterateFunc $ 2 badImul seed
    --let x  = shiftL seed 11
    --let x' = shiftR x 8 + x
    return y -}

main = print "ass"