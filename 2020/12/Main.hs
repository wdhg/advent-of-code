import Data.List (foldl')

data Direction
  = N | S | E | W | L | R | F

data Instruction
  = Instruction Direction Int

data Ship
  = Ship 
    { vector   :: (Int, Int)
    , position :: (Int, Int)
    , waypoint :: (Int, Int)
    } deriving Show

readDirection :: Char -> Direction
readDirection 'N' = N
readDirection 'S' = S
readDirection 'E' = E
readDirection 'W' = W
readDirection 'L' = L
readDirection 'R' = R
readDirection 'F' = F

readInstruction :: String -> Instruction
readInstruction (d:n) = Instruction (readDirection d) (read n)

getIntstructions :: IO [Instruction]
getIntstructions
  = map readInstruction . lines <$> readFile "input"

start :: Ship
start = Ship { vector = dirVector E, position = (0,0), waypoint = (10, 1) }

move :: (Instruction -> Ship -> Ship) -> [Instruction] -> Ship
move f = foldl' (flip f) start

followVector :: Instruction -> Ship -> Ship
followVector (Instruction dir amount) ship
  = case dir of
      L -> turn (-amount) ship
      R -> turn amount ship
      F -> movePosition (vector ship |*| amount) ship
      _ -> movePosition (dirVector dir |*| amount) ship

followWaypoint :: Instruction -> Ship -> Ship
followWaypoint (Instruction dir amount) ship
  = case dir of
      L -> turnWaypoint (-amount) ship
      R -> turnWaypoint amount ship
      F -> movePosition (waypoint ship |*| amount) ship
      _ -> moveWaypoint (dirVector dir |*| amount) ship

turn :: Int -> Ship -> Ship
turn angle ship = ship { vector = rotate (vector ship) angle }

movePosition :: (Int, Int) -> Ship -> Ship
movePosition vec ship = ship { position = position ship |+| vec }

turnWaypoint :: Int -> Ship -> Ship
turnWaypoint angle ship = ship { waypoint = rotate (waypoint ship) angle }

moveWaypoint :: (Int, Int) -> Ship -> Ship
moveWaypoint vec ship = ship { waypoint = waypoint ship |+| vec }

dirVector :: Direction -> (Int, Int)
dirVector N = (0,1)
dirVector S = (0,-1)
dirVector E = (1,0)
dirVector W = (-1,0)

rotate :: (Int, Int) -> Int -> (Int, Int)
rotate vec angle
  | angle > 0 = iterate rotateRight vec !! (angle `div` 90)
  | otherwise = iterate rotateLeft vec !! (-angle `div` 90)

rotateLeft :: (Int, Int) -> (Int, Int)
rotateLeft (x,y) = (-y,x)

rotateRight :: (Int, Int) -> (Int, Int)
rotateRight (x,y) = (y,-x)

(|+|) :: (Int, Int) -> (Int, Int) -> (Int, Int)
(x1,y1) |+| (x2,y2) = (x1 + x2, y1 + y2)

(|-|) :: (Int, Int) -> (Int, Int) -> (Int, Int)
(x1,y1) |-| (x2,y2) = (x1 - x2, y1 - y2)

(|*|) :: (Int, Int) -> Int -> (Int, Int)
(x,y) |*| a = (x * a, y * a)

manhattan :: (Int, Int) -> Int
manhattan (x, y) = abs x + abs y

main :: IO ()
main
  = do
    instructions <- getIntstructions
    print $ manhattan $ position $ move followVector instructions
    print $ manhattan $ position $ move followWaypoint instructions
