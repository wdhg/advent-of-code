getMasses :: IO [Int]
getMasses
  = do
    file <- readFile "input"
    return $ map read (lines file) :: IO [Int]

mapAndSum :: (Int -> Int) -> IO Int
mapAndSum f
  = do
    masses <- getMasses
    return $ sum $ map f masses

calculateFuel :: Int -> Int
calculateFuel mass
  = mass `div` 3 - 2

part1 :: IO Int
part1
  = mapAndSum calculateFuel

part2 :: IO Int
part2
  = mapAndSum calculateTotalFuel
      where
        calculateTotalFuel :: Int -> Int
        calculateTotalFuel
          = sum . tail . takeWhile (>0) . iterate calculateFuel
