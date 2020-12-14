{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative ((<|>))
import Data.Attoparsec.Text hiding (I)
import Data.Bits
import Data.Text (pack, unpack)
import Data.Map (Map, insert, empty, elems)
import Data.List (foldl')

import Prelude hiding (take)

data Instruction
  = Mask (Int, Int) | Mem Int Int deriving Show

allOnes :: Int
allOnes = 2 ^ 36 - 1

buildMasks :: String -> (Int, Int)
buildMasks
  = foldr buildMasks' (0, allOnes) . zip [35,34..0]
    where
      buildMasks' :: (Int, Char) -> (Int, Int) -> (Int, Int)
      buildMasks' (i, c) (ones, zeros)
        = case c of
            '1' -> (ones .|. shiftL 1 i, zeros `xor` shiftL 1 i)
            '0' -> (ones, zeros)
            'X' -> (ones, zeros `xor` shiftL 1 i)


parseMask :: Parser Instruction
parseMask 
  = do
    string "mask = "
    Mask . buildMasks . unpack <$> take 36

parseMem :: Parser Instruction
parseMem
  = do
    string "mem["
    address <- decimal
    string "] = "
    Mem address <$> decimal

parseInstruction :: Parser Instruction
parseInstruction = parseMask <|> parseMem

getInstruction :: String -> Instruction
getInstruction s
  = case parseOnly parseInstruction $ pack s of
      Left e  -> error e
      Right i -> i

getProgram :: IO [Instruction]
getProgram
  = map getInstruction . lines <$> readFile "input"

run :: [Instruction] -> Map Int Int
run = snd . foldl' run' (id, empty)

run' :: (Int -> Int, Map Int Int) -> Instruction -> (Int -> Int, Map Int Int)
run' (_, mem) (Mask (ones, zeros))
  = ((ones .|.) . (zeros .&.), mem)
run' (mask, mem) (Mem addr val)
  = (mask, insert addr (mask val) mem)

runV2 :: [Instruction] -> Map Int Int
runV2 = snd . foldl' runV2' (pure,empty)

runV2' :: (Int -> [Int], Map Int Int) -> Instruction -> (Int -> [Int], Map Int Int)
runV2' (_, mem) (Mask masks@(ones, _))
  = (floating masks . (ones .|.), mem)
runV2' (mask, mem) (Mem addr val)
  = (mask, foldr (`insert` val) mem $ mask addr)

floating :: (Int, Int) -> Int -> [Int]
floating (ones, zeros) addr
  = let mask = allOnes `xor` (ones .|. zeros)
        bits = filter (testBit mask) [35,34..0]
     in foldl' floating' [addr] bits

floating' :: [Int] -> Int -> [Int]
floating' addrs i
  = map (`clearBit` i) addrs ++ map (`setBit` i) addrs

main :: IO ()
main
  = do
    instructions <- getProgram
    print $ sum $ elems $ run instructions
    print $ sum $ elems $ runV2 instructions
