type Layer
  = [Int]

getLayers :: IO [Layer]
getLayers
  = do
    input <- readFile "input"
    let pixels = toInts $ init input
    return $ map (take size) $ takeWhile (not . null) $ iterate (drop size) pixels
      where
        size = 25 * 6
        toInts :: String -> [Int]
        toInts ""
          = []
        toInts (c : cs)
          = read (c : []) : toInts cs

count :: Eq a => a -> [a] -> Int
count value
  = length . filter (== value)

index :: Eq a => a -> [a] -> Int
index value values
  | value `elem` values = length $ takeWhile (/= value) values
  | otherwise           = -1

part1 :: IO Int
part1
  = do
    layers <- getLayers
    let counts = map (count 0) layers
        layer  = layers !! (index (minimum counts) counts)
    return $ (count 1 layer) * (count 2 layer)

part2 :: IO ()
part2
  = do
    layers <- getLayers
    let stackedPixels = [map (!! x) layers | x <- [0..149]]
        finalLayer    = map (head . dropWhile (== 2)) stackedPixels
    putStr $ render finalLayer
      where
        render :: Layer -> String
        render []
          = ""
        render layer
          = (draw $ take 25 layer) ++ ('\n' : (render $ drop 25 layer))
            where
              draw :: [Int] -> String
              draw
                = concatMap drawPixel
              drawPixel :: Int -> String
              drawPixel 0
                = "██"
              drawPixel 1
                = "  "
