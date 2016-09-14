module Common where

import Data.Bits
import Control.Monad
import Data.Word

bitStringToWord :: (Bits a) => String -> a
bitStringToWord = foldl xor zeroBits . (trueBits >=> (\(idx,chr) -> return $ bit idx))
  where trueBits bits = filter (\x ->'1' == snd x) . zip (reverse [0..(length bits - 1)]) $ bits
