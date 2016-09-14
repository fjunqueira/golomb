module Decoder where

import Data.Word
import Data.Bits
import Data.List
import Control.Monad
import Common
import Control.Arrow

decode :: [Word8] -> Int -> [Int]
decode bytes = decodeUntilEmpty (decodeToString bytes)

decodeUntilEmpty :: String -> Int -> [Int]
decodeUntilEmpty [] _ = []
decodeUntilEmpty bits m
   | length bits < 8 && '1' `notElem` bits = []
   | otherwise = bitStringToInt m q (take k rest) : decodeUntilEmpty (drop k rest) m
  where (ones, rest) =  second (drop 1) . span (/= '0') $ bits
        q = length ones
        k = ceiling . logBase 2 $ fromIntegral m

decodeToString :: [Word8] -> String
decodeToString bytes =
  join $ map (\x -> map (\y -> if x `testBit` y then '1' else '0') [7,6..0] ) bytes

bitStringToInt :: Int -> Int -> String -> Int
bitStringToInt m q code = q * m + bitStringToWord code
