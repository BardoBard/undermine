module Binary.Conversion where
  
import Data.List
import Data.Char
import Binary.Basic
import Binary.Operators

-- |converts binary to an int
--
-- e.g "11010" -> 26
--
-- returns: converted int
toDec :: [Char] -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

--toTwos :: [Char] -> Int
toTwos x = binPlus "1" $ inverseBin x
{- assss x 
            | x' > 32 = 
            where 
              x' = length x
  let bin = toBinx 32 x
  if head bin == '1' then fullBinSubtractor bin (decToBase 2 4294967296 "") else bin
 -}
decToBase :: Integral a => a -> a -> ShowS
decToBase base = showIntAtBase base intToDigit

-- | inverses all of the 1 and 0's
--
-- e.g "1110010" -> "0001101"
--
-- returns: inverted binary
inverseBin :: [Char] -> [Char]
inverseBin = map $ \bit -> if bit =='0' then '1'; else '0'

showIntAtBase :: Integral a => a -> (Int -> Char) -> a -> ShowS
showIntAtBase base toChr n0 r0
  | base <= 1 = errorWithoutStackTrace ("Numeric.showIntAtBase: applied to unsupported base " ++ show (toInteger base))
  | n0 <  0   = errorWithoutStackTrace ("Numeric.showIntAtBase: applied to negative number " ++ show (toInteger n0))
  | otherwise = showIt (quotRem n0 base) r0
   where
    showIt (n,d) r = seq c $ -- stricter than necessary
      case n of
        0 -> r'
        _ -> showIt (quotRem n base) r'
     where
      c  = toChr (fromIntegral d)
      r' = c : r
