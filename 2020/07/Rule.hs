module Rule where

import Data.Map

type Bag
  = String

type Rules
  = Map Bag [(Bag, Int)]
