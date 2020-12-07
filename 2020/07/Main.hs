module Main where

import Rule
import Tokens
import Grammer
import Data.List (nub)
import qualified Data.Map as M

getRules :: IO Rules
getRules
  = do
    contents <- readFile "input"
    return $ parse $ tokenize contents

canContain :: Bag -> [(Bag, Int)] -> Bool
canContain bag counts
  = case lookup bag counts of
      Nothing -> False
      Just x  -> x > 0

findCanContain :: Rules -> Bag -> [Bag]
findCanContain rules bag
  = let bags = M.keys $ M.filter (canContain bag) rules
     in findCanContain' rules bags

findCanContain' :: Rules -> [Bag] -> [Bag]
findCanContain' rules bags
  | bags == bags' = bags
  | otherwise     = nub $ findCanContain' rules bags' ++ bags
    where
      bags' = concatMap (findCanContain rules) bags

countMustContain :: Rules -> Bag -> Int
countMustContain rules bag
  = case M.lookup bag rules of
      Nothing -> 0
      Just x  -> sum $ map (\(b, c) -> c + c * countMustContain rules b) x

main :: IO ()
main
  = do
    rules <- getRules
    print $ length $ findCanContain rules "shiny gold"
    print $ countMustContain rules "shiny gold"
