module Encoder where

import Data.Bits
import Data.Word
import Data.List
import Text.Printf
import Data.List.Split
import Data.Maybe
import Control.Monad
import Debug.Trace

encode :: [Int] -> Int -> [Word8]
encode codes m = toWords $ encodedCodes ++ replicate size '0'
  where encodedCodes = encodeToString codes m
        size = 8 - length encodedCodes `mod` 8

encodeToString :: [Int] -> Int -> String
encodeToString [] _ = []
encodeToString (code:rest) m =
  replicate q '0' ++ "1" ++ remainderCode ++ encodeToString rest m
  where (q,r) = code `divMod` m
        b = ceiling . logBase 2 $ fromIntegral m
        remainderCode
          | r < ((2 ^ b) - m) = printf ("%0"++ show (b - 1) ++ "b") r::String
          | otherwise = (printf ("%0"++ show b ++ "b") $ r + (2 ^ b) - m)::String

toWords :: String -> [Word8]
toWords encoded =
   map (foldl xor 0 .
      (\bits -> zip (reverse [0..(length bits - 1)]) bits >>= (\(idx,bit) -> return $ bitToWord bit idx)))
      (chunksOf 8 encoded)

bitToWord :: Char -> Int -> Word8
bitToWord bit index
  | bit == '1' = 0 `setBit` index
  | otherwise = 0
