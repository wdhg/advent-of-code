{-# LANGUAGE FlexibleInstances #-}

import Data.Set
import Data.Maybe (mapMaybe)
import Prelude hiding (map, filter)

class Ord a => Vector a where
  (|+|) :: a -> a -> a
  from2D :: (Int, Int) -> a
  dirs :: Set a

type V3 = (Int, Int, Int)

instance Vector V3 where
  (x1,y1,z1) |+| (x2,y2,z2) = (x1+x2,y1+y2,z1+z2)
  from2D (x,y) = (x,y,0)
  dirs = fromList [ (x,y,z)
                  | x <- [-1..1]
                  , y <- [-1..1]
                  , z <- [-1..1]
                  , (x,y,z) /= (0,0,0)
                  ]

type V4 = (Int, Int, Int, Int)

instance Vector V4 where
  (x1,y1,z1,w1) |+| (x2,y2,z2,w2) = (x1+x2,y1+y2,z1+z2,w1+w2)
  from2D (x,y) = (x,y,0,0)
  dirs = fromList [ (x,y,z,w)
                  | x <- [-1..1]
                  , y <- [-1..1]
                  , z <- [-1..1]
                  , w <- [-1..1]
                  , (x,y,z,w) /= (0,0,0,0)
                  ]

newtype World a = World (Set a) deriving Show

f :: Vector a => (Int, Int, Char) -> Maybe a
f (y,x,'#') = Just $ from2D (x,y)
f _         = Nothing

getWorld :: Vector a => IO (World a)
getWorld
  = do
    rows <- lines <$> readFile "input"
    let size = length rows `div` 2
        is = [-size..size]
        active = concat $ zipWith (\i row -> zip3 (repeat i) is row) is rows
    return $ World $ fromList $ mapMaybe f active

step :: Vector a => World a -> World a
step world = fold (stepCell world) world $ cellsToStep world

adjacentTo :: Vector a => a -> Set a
adjacentTo v = map (|+| v) dirs

cellsToStep :: Vector a => World a -> Set a
cellsToStep (World active)
  = union active $ unions $ map adjacentTo active

stepCell :: Vector a => World a -> a -> World a -> World a
stepCell worldRead@(World activeRead) v (World activeWrite)
  | active && ns /= 2 && ns /= 3 = World $ delete v activeWrite
  | not active && ns == 3        = World $ insert v activeWrite
  | otherwise                    = World activeWrite
    where
      active = v `member` activeRead
      ns = neighbors worldRead v

neighbors :: Vector a => World a -> a -> Int
neighbors (World active) v
  = size $ filter (`member` active) $ adjacentTo v


simulate :: Vector a => Int -> World a -> World a
simulate n w
  | n <= 0    = w
  | otherwise = simulate (n - 1) $ step w

main :: IO ()
main
  = do
    initial3 <- getWorld :: IO (World V3)
    initial4 <- getWorld :: IO (World V4)
    let (World active3) = simulate 6 initial3
        (World active4) = simulate 6 initial4
    print $ size active3
    print $ size active4
