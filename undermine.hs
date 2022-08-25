{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Undermine where

import Data.Function

import System.IO

import Data.Char

import Data.List

import Data.Maybe

import Control.Monad

import qualified Data.Text as T

--my libs

import Basic

import Binary.Basic

import Binary.Shift

import Binary.Logicgates
import Binary.Operators
import Binary.Conversion
import Json.Parser

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


items = do
    index'  <- index
    weight' <- weight
    return $ replace' index' weight' [0..119]


{- repeatNTimes 0 _    = "s"
repeatNTimes n seed = do
  masterindex' <- masterindex seed
  repeatNTimes (n-1) (seed + 1) -}

idkman :: [Int] -> Int -> Int
idkman (x:xs) y = do
    if y <= 0
        then 0
        else 1 + idkman xs (y - x)

output seed = do
    items' <- items
    masterindex (idkman items' (range 1 825 seed))