module Main where

import Passport
import Tokens
import Data.List.Split (splitOn)

getPassports :: IO [Passport]
getPassports
  = do
    contents <- readFile "input"
    return $ map (fromTokens . tokenize) $ splitOn "\n\n" contents

main :: IO ()
main
  = do
    passports <- getPassports
    print $ length (filter isValid passports) - 1
