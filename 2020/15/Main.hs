import Data.Map (Map(..), fromList, lookup, insert)
import Data.List (foldl')

import Prelude hiding (lookup)

play :: Int -> [Int] -> Int
play i xs
  = let is = fromList $ zip (init xs) [0..]
     in fst $ foldl' play' (last xs, is) [length xs..i - 1]

play' :: (Int, Map Int Int) -> Int -> (Int, Map Int Int)
play' (x, is) i
  = let is' = insert x (i-1) is
     in case lookup x is of
          Nothing -> (0, is')
          Just j  -> ((i-1)-j, is')

main :: IO ()
main
  = do
    let xs = [1,0,18,10,19,6]
    print $ play 2020 xs
    print $ play 30000000 xs
