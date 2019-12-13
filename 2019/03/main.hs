type Vector    = (Int, Int) -- for change of position only
type Point     = Vector -- for position only
type Segment   = (Vector, Vector) -- (position, vector out)
type Rectangle = (Point, Point) -- bottom left and top right corners
type Wire      = [Segment]

-- seperates string on first newline
seperateLines :: String -> (String, String)
seperateLines (c : cs)
  | c == '\n' = ("", cs)
  | otherwise = (c : before, after)
    where
      (before, after) = seperateLines cs

-- splits up string on commas
split :: String -> [String]
split ""
  = [""]
split (c : cs)
  | c == ','  = "" : after
  | otherwise = (c : first) : remaining
    where
      after@(first : remaining) = split cs

cmdToVector :: String -> Vector
-- Pre: input is of form "L204"
cmdToVector (dir : lenString)
  = case dir of
      'U' -> (   0,  len)
      'D' -> (   0, -len)
      'L' -> (-len,    0)
      'R' -> ( len,    0)
    where
      len = read lenString

getWires :: IO (Wire, Wire)
getWires
  = do
    input <- readFile "input"
    let (cmds1, cmds2) = seperateLines input
    return $ (convert cmds1, convert cmds2)
      where
        convert :: String -> Wire
        convert commands
          = zip points vectors
            where
              vectors = map cmdToVector $ split commands
              points  = (0, 0) : scanl1 add vectors

add :: Vector -> Vector -> Vector
add (x1, y1) (x2, y2)
  = (x1 + x2, y1 + y2)

segToRect :: Segment -> Rectangle
segToRect (point, vector@(dx, dy))
  | dx < 0 || dy < 0 = (point `add` vector, point)
  | otherwise        = (point, point `add` vector)

intersects :: Segment -> Segment -> Bool
intersects segment1 segment2
  = intersects' (segToRect segment1) (segToRect segment2)
    where
      intersects' :: Rectangle -> Rectangle -> Bool
      intersects' ((x11, y11), (x12, y12)) ((x21, y21), (x22, y22))
        = not (isToTheSide || isAbove)
          where
            isToTheSide = (x12 < x21) || (x22 < x11)
            isAbove     = (y12 < y21) || (y22 < y11)

getCrossover :: Segment -> Segment -> Point
getCrossover ((x1, y1), (dx1, dy1)) ((x2, y2), (dx2, dy2))
  | dx1 == 0 || dy2 == 0 = (x1, y2)
  | otherwise            = (x2, y1)

getCrossovers :: Wire -> Wire -> [Point]
getCrossovers wire1 wire2
  = filter (/= (0, 0)) $ concatMap (getCrossovers' wire2) wire1
    where
      getCrossovers' :: Wire -> Segment -> [Point]
      getCrossovers' wire segment
        = map (getCrossover segment) $ filter (intersects segment) wire

manhattan :: Point -> Int
manhattan (x, y)
  = (abs x) + (abs y)

contains :: Segment -> Point -> Bool
contains segment (x, y)
  = x1 <= x && y1 <= y && x <= x2 && y <= y2
    where
      ((x1, y1), (x2, y2)) = segToRect segment

distance :: Point -> Point -> Int
distance (x1, y1) (x2, y2)
  = manhattan (x2 - x1, y2 - y1)

stepsTo :: Wire -> Point -> Int
stepsTo [] _
  = error "ruh ruh"
stepsTo (segment@(point, vector) : wire) target
  | segment `contains` target = distance point target
  | otherwise                 = (manhattan vector) + (stepsTo wire target)

part1 :: IO Int
part1
  = do
    (wire1, wire2) <- getWires
    return $ minimum $ map manhattan $ getCrossovers wire1 wire2

part2 :: IO Int
part2
  = do
    (wire1, wire2) <- getWires
    let crossovers = getCrossovers wire1 wire2
        wire1Steps = map (stepsTo wire1) crossovers
        wire2Steps = map (stepsTo wire2) crossovers
    return $ minimum $ zipWith (+) wire1Steps wire2Steps
