data Square
  = Empty | Tree | OutOfBounds
    deriving (Show, Eq)

data Slope
  = Slope (Int, Int) [[Square]]

readSquare :: Char -> Square
readSquare '.' = Empty
readSquare '#' = Tree

getSlope :: IO Slope
getSlope
  = do
    contents <- readFile "input"
    let squares = map (map readSquare) $ lines contents
        width = length $ head squares
        height = length squares
    return $ Slope (width, height) squares

countTrees :: (Int, Int) -> Slope -> Int
countTrees
  = countTrees' (0, 0)

countTrees' :: (Int, Int) -> (Int, Int) -> Slope -> Int
countTrees' (x, y) vector@(dx, dy) slope
  | square == OutOfBounds = 0
  | square == Tree        = 1 + remaining
  | otherwise             = remaining
    where
      newPos = (x + dx, y + dy)
      square = getSquare newPos slope
      remaining = countTrees' newPos vector slope

getSquare :: (Int, Int) -> Slope -> Square
getSquare (x, y) (Slope (width, height) squares)
  | y >= height = OutOfBounds
  | otherwise  = squares !! y !! (x `mod` width)

main :: IO ()
main
  = do
    slope <- getSlope
    let counts = map (`countTrees` slope)
                 [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
    print $ counts !! 1
    print $ product counts
