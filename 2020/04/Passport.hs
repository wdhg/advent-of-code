{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Passport where

newtype Year = Year Int deriving (Show, Num, Eq, Ord)
data Length = CM Int | IN Int deriving Show
newtype Hex = Hex String deriving Show
data Color = AMB | BLU | BRN | GRY | GRN | HZL | OTH deriving Show
newtype Id = Id Int deriving Show
data Passport
  = Passport
    { byr :: Maybe Year
    , iyr :: Maybe Year
    , eyr :: Maybe Year
    , hgt :: Maybe Length
    , hcl :: Maybe Hex
    , ecl :: Maybe Color
    , pid :: Maybe Id
    } deriving Show

between :: Year -> (Year, Year) -> Bool
between x (lower, upper)
  = x >= lower && x <= upper

lengthValid :: Length -> Bool
lengthValid (CM x) = x >= 150 && x <= 193
lengthValid (IN x) = x >= 59 && x <= 76

empty :: Passport
empty = Passport Nothing Nothing Nothing Nothing Nothing Nothing Nothing

isValid :: Passport -> Bool
isValid (Passport (Just b) (Just i) (Just e) (Just l) (Just _) (Just _) (Just _))
  = (b `between` (1920, 2002)) && (i `between` (2010, 2020)) && (e `between` (2020, 2030)) && lengthValid l
isValid _
  = False
