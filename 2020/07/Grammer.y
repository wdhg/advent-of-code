{
module Grammer where

import Tokens
import Rule
import Data.Map (empty, insert)
}

%name parse
%tokentype { Token }
%error { parseError }

%token
  bag      { TokenBag $$ }
  contain  { TokenContain }
  noBags   { TokenNoBags }
  nBags    { TokenNBags $$ }
  comma    { TokenComma }
  period   { TokenPeriod }

%%

Rules
  : bag contain Counts Rules { insert $1 $3 $4 }
  | {- empty -}              { empty }

Counts
  : nBags bag comma Counts { ($2, $1) : $4 }
  | nBags bag period       { [($2, $1)] }
  | noBags period          { [] }
  | {- empty -}            { [] }

{
parseError :: [Token] -> a
parseError ts = error $ "Error parsing " ++ show ts
}
