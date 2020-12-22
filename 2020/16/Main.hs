{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative (some)
import Data.Attoparsec.Text
import Data.Text (Text(..), unpack, pack)

newtype Ticket
  = Ticket [Int] deriving Show

data Restriction
  = Restriction String (Int, Int) (Int, Int) deriving (Show, Eq)

parseRange :: Parser (Int, Int)
parseRange
  = do
    left <- decimal
    char '-'
    right <- decimal
    return (left, right)

parseRestriction :: Parser Restriction
parseRestriction
  = do
    name <- takeWhile1 (/= ':')
    string ": "
    left <- parseRange
    string " or "
    right <- parseRange
    skipSpace
    return $ Restriction (unpack name) left right

parseTicket :: Parser Ticket
parseTicket
  = do
    xs <- sepBy decimal (char ',')
    char '\n'
    return $ Ticket xs

parseInput :: Parser ([Restriction], Ticket, [Ticket])
parseInput
  = do
    restrictions <- some parseRestriction
    skipSpace
    string "your ticket:"
    skipSpace
    ours <- parseTicket
    skipSpace
    string "nearby tickets:"
    skipSpace
    nearby <- some parseTicket
    skipSpace
    return (restrictions, ours, nearby)

getInput :: IO ([Restriction], Ticket, [Ticket])
getInput
  = do
    content <- readFile "input"
    case parseOnly parseInput $ pack content of
      Left e      -> error e
      Right input -> return input

inRange :: (Int, Int) -> Int -> Bool
inRange (l, u) x
  = l <= x && x <= u

satisfies :: Int -> Restriction -> Bool
satisfies x (Restriction _ left right)
  = inRange left x || inRange right x

invalidFields :: [Restriction] -> Ticket -> [Int]
invalidFields restrictions (Ticket xs)
  = filter (\x -> not $ any (x `satisfies`) restrictions) xs

sortFields :: [Ticket] -> [Restriction] -> [Restriction]
sortFields ts rs
  = map head $ until (all lengthOne) sortFields' $ potentialFields ts rs

sortFields' :: Eq a => [[a]] -> [[a]]
sortFields' rss
  = let fixed = getFixed rss
     in map (`fix` fixed) rss

fix :: Eq a => [a] -> [a] -> [a]
fix [r] _    = [r]
fix rs fixed = filter (`notElem` fixed) rs

getFixed :: [[a]] -> [a]
getFixed = concat . filter lengthOne

lengthOne :: [a] -> Bool
lengthOne = (== 1) . length

potentialFields :: [Ticket] -> [Restriction] -> [[Restriction]]
potentialFields ts rs
  = map (`possible` rs) xsByField
    where
      xsByTicket = map (\(Ticket xs) -> xs) ts
      xsByField = map (\i -> map (!! i) xsByTicket) [0..length rs - 1]

possible :: [Int] -> [Restriction] -> [Restriction]
possible xs
  = filter (\r -> all (`satisfies` r) xs)

f :: Int -> Restriction -> Int
f x (Restriction name _ _)
  | head (words name) == "departure" = x
  | otherwise = 1

main :: IO ()
main
  = do
    (restrictions, ours@(Ticket xs), nearby) <- getInput
    print $ sum $ concatMap (invalidFields restrictions) $ ours : nearby
    let valid = ours : filter (null . invalidFields restrictions) nearby
        fields = sortFields valid restrictions
    print $ product $ zipWith f xs fields
