module Encoder where

import Data.Bits
import Data.Word
import Data.List
import Text.Printf
import Data.List.Split
import Data.Maybe
import Control.Monad

encode :: [Int] -> Int -> [Word8]
encode codes m = toWords encodedCodes
  where encodedCodes = encodeToString codes m

encodeToString :: [Int] -> Int -> String
encodeToString [] _ = []
encodeToString (code:rest) m =
  replicate q '1' ++ "0" ++ remainderCode ++ encodeToString rest m
  where (q,r) = code `divMod` m
        b = ceiling . logBase 2 $ fromIntegral m
        remainderCode
          | r < ((2 ^ b) - m) = printf ("%0"++ show (b - 1) ++ "b") r::String
          | otherwise = (printf ("%0"++ show b ++ "b") $ r + (2 ^ b) - m)::String

encodedSizeInBits :: Int -> Int -> Int
encodedSizeInBits code m = q + restLength + 1
  where (q,r) = code `divMod` m
        b = ceiling . logBase 2 $ fromIntegral m
        restLength = if r < ((2 ^ b) - m) then b - 1 else b

toWords :: String -> [Word8]
toWords encoded =
   map (foldl xor 0 .
      (\bits -> zip (reverse [0..(length bits - 1)]) bits >>= (\(idx,bit) -> return $ bitToWord bit idx)))
      (chunksOf 8 encoded)

bitToWord :: Char -> Int -> Word8
bitToWord bit index
  | bit == '1' = 0 `setBit` index
  | otherwise = 0
