getMasses :: IO [Int]
getMasses
  = do
    file <- readFile "input"
    return $ map read (lines file) :: IO [Int]

calculateFuels :: [Int] -> [Int]
calculateFuels
  = filter (>0) . map (\x -> x `div` 3 - 2)

calculateAllFuels :: [Int] -> [Int] 
calculateAllFuels
  = map sum . takeWhile (not . null) . tail . iterate calculateFuels

perform :: ([Int] -> [Int]) -> IO Int
perform f
  = do
    masses <- getMasses
    return $ sum $ f masses

part1 :: IO Int
part1
  = perform calculateFuels

part2 :: IO Int
part2
  = perform calculateAllFuels
