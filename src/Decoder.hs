module Decoder where

import Data.Word
import Data.Bits
import Data.List
import Control.Monad

decode :: [Word8] -> Int -> [Int]
decode bytes = decodeUntilEmpty (decodeToString bytes)

decodeUntilEmpty :: String -> Int -> [Int]
decodeUntilEmpty [] _ = []
decodeUntilEmpty bits m =
  bitStringToInt m bitsToTake (take bitsToTake rest) : decodeUntilEmpty (drop bitsToTake rest) m
  where (ones, rest) = span (/= '0') . dropWhile (=='0') $ bits
        bitsToTake = length ones

decodeToString :: [Word8] -> String
decodeToString bytes =
  join $ map (\x -> map (\y -> if x `testBit` y then '1' else '0') [7,6..0] ) bytes

--fix this
bitStringToInt :: Int -> Int -> String -> Int
bitStringToInt m q code = q * m + r
--remove duplicate
  where r = foldl xor 0 . (\bits -> zip (reverse [0..(length bits - 1)]) bits >>= (\(idx,bit) -> return $ bitToWord bit idx)) $ code

--remove duplicate
bitToWord :: Char -> Int -> Int
bitToWord bit index
  | bit == '1' = 0 `setBit` index
  | otherwise = 0
