module Binary.Shift where

import Basic

import Binary.Basic

import Binary.Logicgates
import Binary.Conversion
import Binary.Operators


-- |shifts a signed int32 n places (in binary) to the left
--
-- for binary conversion use shiftSB2BL32
--
-- returns: shifted int
shiftSDecL32 :: Integral a => Int -> a -> Int
shiftSDecL32 n dec = do
    let bin = toBin32 $ shiftDecToBinL n dec;
    if head bin == '1' then toDec bin - 4294967296 else toDec bin

-- |shifts a signed int32 n places (in binary) to the right
--
-- e.g 
-- for binary conversion use shiftSB2BR32
--
-- returns: shifted int
shiftUDecR32 :: Integral a => Int -> a -> Int
shiftUDecR32 n dec = do
    let bin = toBin32 $ shiftDecToBinR n dec;
    if head bin == '1' then toDec bin - 4294967296 else toDec bin

-- |shifts a signed int32 n places (in binary) to the left
--
-- for int conversion use shiftUDecL32
--
-- returns: shifted binary
shiftSB2BL32 :: Int -> [Char] -> [Char]
shiftSB2BL32 n list = toBinx 32 $ shiftBinL n list;

-- |shifts a signed int32 n places (in binary) to the left
--
-- for int conversion use shiftUDecR32
--
-- returns: shifted binary
shiftUB2BR32 :: Int -> [Char] -> [Char]
shiftUB2BR32 n list = toBinx 32 $ shiftBinR n list;



shiftR z x y = take z (decToBase 2 x y) ++ replicate z '0'

shiftDecToBinL :: Integral a => Int -> a -> [Char]
shiftDecToBinL z x =  decToBase 2 x $ replicate z '0'

shiftDecToBinR :: Integral a => Int -> a -> [Char]
shiftDecToBinR z x = shiftBinR z $ decToBase 2 x ""

shiftBinL :: Int -> [Char] ->  [Char]
shiftBinL n list = list ++ replicate n '0'

shiftBinR :: Int -> [Char] ->  [Char]
shiftBinR n list = "0" ++ pop n list