import Control.Monad
import Control.Monad.ST
import Data.Array
import Data.Array.ST
import Data.List.Split (chunksOf)

type Seats
  = Array (Int, Int) Char

type STSeats s
  = STArray s (Int, Int) Char

getSeats :: IO Seats
getSeats
  = do
    contents <- readFile "input"
    let ls = lines contents
        width = length ls
        height = length $ head ls
    return $ listArray ((0,0),(width-1,height-1)) $ filter (/='\n') contents

simulate :: Seats -> Seats
simulate seats
  | seats == seats' = seats'
  | otherwise = simulate seats'
    where
      seats' = step seats

step :: Seats -> Seats
step seats 
  = runSTArray $ do
    seats' <- thaw seats
    bounds <- getBounds seats'
    forM_ (range bounds) (stepSeat seats seats')
    return seats'

stepSeat :: Seats -> STSeats s -> (Int, Int) -> ST s ()
stepSeat seats seats' pos
  = case seats ! pos of
      'L' | neighbors == 0 -> writeArray seats' pos '#'
      '#' | neighbors >= 4 -> writeArray seats' pos 'L'
      _                    -> pure ()
    where
      neighbors = countNeighbors seats pos

simulate' :: Seats -> Seats
simulate' seats
  | seats == seats' = seats'
  | otherwise = simulate' seats'
    where
      seats' = step' seats

step' :: Seats -> Seats
step' seats 
  = runSTArray $ do
    seats' <- thaw seats
    bounds <- getBounds seats'
    forM_ (range bounds) (stepSeat' seats seats')
    return seats'

stepSeat' :: Seats -> STSeats s -> (Int, Int) -> ST s ()
stepSeat' seats seats' pos
  = case seats ! pos of
      'L' | visible == 0 -> writeArray seats' pos '#'
      '#' | visible >= 5 -> writeArray seats' pos 'L'
      _                  -> pure ()
    where
      visible = countVisible seats pos

directions :: [(Int,Int)]
directions = [(-1,-1),(-1,0),(-1,1),(0,-1),(0,1),(1,-1),(1,0),(1,1)]

countNeighbors :: Seats -> (Int, Int) -> Int
countNeighbors seats pos
  = length $ filter (isOccupied seats pos) directions

countVisible :: Seats -> (Int, Int) -> Int
countVisible seats pos
  = length $ filter (ray seats pos) directions

ray :: Seats -> (Int, Int) -> (Int, Int) -> Bool
ray seats (x,y) dir@(dx,dy)
  | outOfBounds (snd $ bounds seats) pos = False
  | seats ! pos == '.' = ray seats pos dir
  | otherwise = seats ! pos == '#'
    where
      pos = (x + dx, y + dy)

isOccupied :: Seats -> (Int, Int) -> (Int, Int) -> Bool
isOccupied seats (x,y) (dx,dy)
  | outOfBounds (snd $ bounds seats) pos = False
  | otherwise = seats ! pos == '#'
    where
      pos = (x + dx, y + dy)

outOfBounds :: (Int, Int) -> (Int, Int) -> Bool
outOfBounds (w,h) (x,y)
  = x < 0 || y < 0 || x > w || y > h

showSeats :: Seats -> IO ()
showSeats seats
  = let (_,(_,h)) = bounds seats
     in putStr $ unlines $ chunksOf (h+1) $ elems seats

main :: IO ()
main
  = do
    seats <- getSeats
    let seats' = simulate' seats
    showSeats seats'
    print $ length $ filter (== '#') $ elems seats'
