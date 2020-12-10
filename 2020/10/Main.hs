import Data.List (sort)
import Data.Map (Map(..), empty, insert, lookup)
import Control.Monad.State
import Prelude hiding (lookup)

getNumbers :: IO [Int]
getNumbers
  = do
    contents <- readFile "input"
    let xs = sort $ map read $ lines contents
    return $ 0 : xs ++ [maximum xs + 3]

differences :: [Int] -> [Int]
differences xs
  = zipWith (-) (tail xs) (init xs)

count :: Int -> [Int] -> Int
count x
  = length . filter (== x)

arrangements :: [Int] -> Int
arrangements xs
  = evalState (arrangements' xs) empty

arrangements' :: [Int] -> State (Map [Int] Int) Int
arrangements' xs
  = do
    n <- checkCache xs
    case n of
      Just n  -> return n
      Nothing -> do
        n <- case xs of
               [_] -> return 1
               (1:1:xs') -> liftM2 (+) (arrangements' (1:xs')) (arrangements' (2:xs'))
               (1:2:xs') -> liftM2 (+) (arrangements' (2:xs')) (arrangements' (3:xs'))
               (2:1:xs') -> liftM2 (+) (arrangements' (1:xs')) (arrangements' (3:xs'))
               (_:xs')   -> arrangements' xs'
        cacheResult xs n
        return n

checkCache :: [Int] -> State (Map [Int] Int) (Maybe Int)
checkCache xs
  = state $ \cache -> (lookup xs cache, cache)

cacheResult :: [Int] -> Int -> State (Map [Int] Int) ()
cacheResult xs n
  = state $ \cache -> ((), insert xs n cache)

main :: IO ()
main
  = do
    numbers <- getNumbers
    let diffs = differences numbers
        ones = count 1 diffs
        threes = count 3 diffs
    print $ ones * threes
    print $ arrangements diffs
