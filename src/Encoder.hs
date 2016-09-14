module Encoder where

import Data.Bits
import Data.Word
import Data.List
import Text.Printf
import Data.List.Split
import Data.Maybe
import Control.Monad
import Common

encode :: [Int] -> Int -> [Word8]
encode codes m = toWords $ encodedCodes ++ replicate complement '0'
  where encodedCodes = encodeToString codes m
        missingBits = length encodedCodes `mod` 8
        complement = if missingBits > 0 then 8 - missingBits else 0

encodeToString :: [Int] -> Int -> String
encodeToString [] _ = []
encodeToString (code:rest) m =
  replicate q '1' ++ "0" ++ remainderCode ++ encodeToString rest m
  where (q,r) = code `divMod` m
        k = ceiling . logBase 2 $ fromIntegral m
        remainderCode = printf ("%0"++ show k ++ "b") r::String

toWords :: String -> [Word8]
toWords encoded = map bitStringToWord $ chunksOf 8 encoded
