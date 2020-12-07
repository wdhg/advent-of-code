{
module Tokens where
}

%wrapper "basic"

$digit = [0-9]
$alpha = [a-zA-Z]

tokens :-

  no\ other\ bags          { \_ -> TokenNoBags }
  $alpha+\ $alpha+\ bags?  { \s -> TokenBag $ unwords $ take 2 $ words s }
  contain                  { \_ -> TokenContain }
  $digit                   { \s -> TokenNBags $ read s }
  \,                       { \s -> TokenComma }
  \.                       { \_ -> TokenPeriod }
  $white                   ;

{

data Token
  = TokenBag String
  | TokenContain
  | TokenNoBags
  | TokenNBags Int
  | TokenComma
  | TokenPeriod
    deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize
  = alexScanTokens
}
