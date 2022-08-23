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


--this works every time
range :: Int -> Int -> Int -> Int
range min max seed = nextUInt seed `mod` (max - min + 1) + min



nextUInt seed = do
    let seedBin = decToBase 2 seed ""
    let x     = xorB' seedBin $ shiftBinL32 11 seedBin --add if '1' then two else continue
    let x'    = xorB' x       $ shiftBinR32 8 x
    let y     = decToBase 2   (iterateFunc  3 badImul' seed) ""
    let y'    = xorB' y       $ shiftBinR32 19 y
    toDec (xorB' x' y')

main = print "oop-"