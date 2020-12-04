{
module Tokens where

import Passport
import Data.Char (isDigit)
}

%wrapper "basic"

$digit = [0-9]
$hex = [0-9a-f]

tokens :-

  byr\:$digit{4}   {\s -> TokenBYR $ Year $ read $ drop 4 s}
  iyr\:$digit{4}   {\s -> TokenIYR $ Year $ read $ drop 4 s}
  eyr\:$digit{4}   {\s -> TokenEYR $ Year $ read $ drop 4 s}
  hgt\:$digit{3}cm {\s -> TokenHGT $ CM $ read $ take 3 $ drop 4 s}
  hgt\:$digit{2}in {\s -> TokenHGT $ IN $ read $ take 2 $ drop 4 s}
  hcl\:\#$hex{6}   {\s -> TokenHCL $ Hex $ drop 5 s}
  ecl\:amb         {\_ -> TokenECL AMB}
  ecl\:blu         {\_ -> TokenECL BLU}
  ecl\:brn         {\_ -> TokenECL BRN}
  ecl\:gry         {\_ -> TokenECL GRY}
  ecl\:grn         {\_ -> TokenECL GRN}
  ecl\:hzl         {\_ -> TokenECL HZL}
  ecl\:oth         {\_ -> TokenECL OTH}
  pid\:$digit{9}   {\s -> TokenPID $ Id $ read $ drop 4 s}
  $white+          ;
  .                ;

{

data Token
  = TokenBYR Year
  | TokenIYR Year
  | TokenEYR Year
  | TokenHGT Length
  | TokenHCL Hex
  | TokenECL Color
  | TokenPID Id
  | TokenWhite
    deriving Show

tokenize :: String -> [Token]
tokenize
  = alexScanTokens

fromTokens :: [Token] -> Passport
fromTokens []
  = empty
fromTokens (t:ts)
  = case t of
      TokenBYR x -> passport {byr = Just x}
      TokenIYR x -> passport {iyr = Just x}
      TokenEYR x -> passport {eyr = Just x}
      TokenHGT x -> passport {hgt = Just x}
      TokenHCL x -> passport {hcl = Just x}
      TokenECL x -> passport {ecl = Just x}
      TokenPID x -> passport {pid = Just x}
    where
      passport = fromTokens ts
}
