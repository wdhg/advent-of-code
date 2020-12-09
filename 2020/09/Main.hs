getNumbers :: IO [Int]
getNumbers
  = do
    contents <- readFile "input"
    return $ map read $ lines contents

firstInvalid :: [Int] -> Int
firstInvalid xs
  | y `elem` sums = firstInvalid $ drop 1 xs
  | otherwise     = y
    where
      (preamble, y:_) = splitAt 25 xs
      sums = [x + y | x <- preamble, y <- preamble]

findWeakness :: [Int] -> Int -> Int
findWeakness xs invalid
  | invalid `elem` sums = maximum values + minimum values
  | otherwise = findWeakness (tail xs) invalid
    where
      values = take (length sums) xs
      sums = takeWhile (<= invalid) $ scanl1 (+) xs

main :: IO ()
main
  = do
    numbers <- getNumbers
    let invalid = firstInvalid numbers
    print invalid
    print $ findWeakness numbers invalid
