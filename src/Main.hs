module Main where

import System.Exit(exitFailure)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import Control.Monad (when, liftM2, unless, join)
import qualified Data.ByteString as B
import Data.Word
import Encoder
import Decoder
import Text.Printf
import Data.List.Split

data Arguments = Arguments { inputFile :: String, outputFile :: String, dictionary :: String, m :: Int} deriving (Show)

main :: IO ()
main =  fmap head getArgs >>= \input ->
        case input of
          "encode" -> getArgs >>= codify . tail
          "decode" -> getArgs >>= decodify . tail
          _ -> do
            putStrLn "Usage:"
            putStrLn "Note: m defaults to 4"
            putStrLn "golomb encode --input FILENAME --output FILENAME --m INTEGER"
            putStrLn "golomb decode --input FILENAME --m INTEGER"

decodify :: [String] -> IO ()
decodify arguments = do

  let args = parseInput . chunksOf 2 $ arguments

  doesFileExist (inputFile args) >>= \exists -> unless exists $
    putStrLn ("File '" ++ inputFile args ++ "' not found!") >> exitFailure

  encodedContent <- B.readFile $ inputFile args

  putStr $ codeToChar getAlphabet <$> decode (B.unpack encodedContent) (m args)

codify :: [String] -> IO ()
codify arguments = do

  let args = parseInput . chunksOf 2 $ arguments

  doesFileExist (inputFile args) >>= \exists -> unless exists $
    putStrLn ("File '" ++ inputFile args ++ "' not found!") >> exitFailure

  contents <- readFile $ inputFile args

  let codeAlphabet = charToCode getAlphabet <$> contents

  let encodedContent = encode codeAlphabet (m args)

  let sizes = map ((\x -> length . encodeToString x $ m args) . (: [])) codeAlphabet

  B.writeFile (outputFile args) $ B.pack encodedContent

  putStrLn "Character -> Encoding Map\n"
  print $ prettyFormat contents sizes (join $ fmap (\word -> printf "%08b" word ::String) encodedContent)
  putStrLn ""

  putStrLn "Original text (in hex)\n"
  print $ fmap (\word -> printf "0x%08x" word ::String) contents
  putStrLn $ "\n Length (in bytes): " ++ show (length contents) ++ "\n"

  putStrLn "Encoded text (in hex)\n"
  print $ fmap (\word -> printf "0x%08x" word ::String) encodedContent
  putStrLn $ "\n Length (in bytes): " ++ show (length encodedContent) ++ "\n"

prettyFormat :: String -> [Int] -> String -> [(Char,String)]
prettyFormat [] _ _ = []
prettyFormat (y:ys) (x:xs) binaryString = (y,take x binaryString) : prettyFormat ys xs (drop x binaryString)

getAlphabet :: [(Char,Int)]
getAlphabet = zip (['\r','\n',' ','.','!','?',','] ++ ['a'..'z']) [0..]

charToCode :: [(Char,Int)] -> Char -> Int
charToCode alphabet char = let (_,code) = tryGetCode . filter (\(element,_) -> char == element) $ alphabet in code
  where tryGetCode [] = error (char : " is unknown")
        tryGetCode (x:_) = x

codeToChar :: [(Char,Int)] -> Int -> Char
codeToChar alphabet code = let (char,_) = tryGetChar . filter (\(_,element) -> code == element) $ alphabet in char
  where tryGetChar [] = error (show code ++ " is unknown")
        tryGetChar (x:_) = x

parseInput :: [[String]] -> Arguments
parseInput (["--input", file]:rest) = Arguments file (outputFile arguments) (dictionary arguments) (m arguments)
  where arguments = parseInput rest
parseInput (["--output", file]:rest) = Arguments (inputFile arguments) file (dictionary arguments) (m arguments)
  where arguments = parseInput rest
parseInput (["--dictionary", file]:rest) = Arguments (inputFile arguments) (outputFile arguments) file (m arguments)
  where arguments = parseInput rest
parseInput (["--m", file]:rest) = Arguments (inputFile arguments) (outputFile arguments) (dictionary arguments) $ read file
  where arguments = parseInput rest
parseInput _ = Arguments "" "encoded" "" 4
