module Decoder where

import Data.Word
import Data.Bits
import Data.List
import Control.Monad

decode :: [Word8] -> Int -> [Int]
decode bytes = decodeUntilEmpty (decodeToString bytes)

decodeUntilEmpty :: String -> Int -> [Int]
decodeUntilEmpty [] _ = []
decodeUntilEmpty bits m
   | length bits < 8 && '1' `notElem` bits = []
   | otherwise = bitStringToInt m q (take k (drop 1 rest)) : decodeUntilEmpty (drop k (drop 1 rest)) m
  where (ones, rest) = span (/= '0') bits
        q = length ones
        k = ceiling . logBase 2 $ fromIntegral m

decodeToString :: [Word8] -> String
decodeToString bytes =
  join $ map (\x -> map (\y -> if x `testBit` y then '1' else '0') [7,6..0] ) bytes

--fix this
bitStringToInt :: Int -> Int -> String -> Int
bitStringToInt m q code = q * m + r
--remove duplicate
  where r = foldl xor 0 . (trueBits >=> (\(idx,chr) -> return $ bit idx)) $ code
        trueBits bits = filter (\x ->'1' == snd x) . zip (reverse [0..(length bits - 1)]) $ bits
