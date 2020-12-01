import Data.List (find)

getNumbers :: IO [Int]
getNumbers
  = do
    contents <- readFile "input"
    return $ map read $ lines contents

findPairAddsTo :: [Int] -> Int -> Maybe (Int, Int)
findPairAddsTo [] _
  = Nothing
findPairAddsTo (x:xs) total
  = case find (== (total - x)) xs of
      Just y  -> Just (x, y)
      Nothing -> findPairAddsTo xs total

part1 :: [Int] -> Int
part1 xs
  = case findPairAddsTo xs 2020 of
      Just (x, y) -> x * y
      Nothing -> error "No pair adds to 2020"

part2 :: [Int] -> Int
part2 []
  = error "No triple adds to 2020"
part2 (x:xs)
  = case findPairAddsTo xs (2020 - x) of
      Just (y, z) -> x * y * z
      Nothing     -> part2 xs

main :: IO ()
main
  = do
    numbers <- getNumbers
    print $ part1 numbers
    print $ part2 numbers
