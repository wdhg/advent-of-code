{-# LANGUAGE TupleSections #-}

import Data.Maybe (catMaybes)
import Data.List (foldl')
import Data.List.Split (splitOn)

data Buses
  = Buses Integer [Maybe Integer] deriving Show

readBusId :: String -> Maybe Integer
readBusId "x" = Nothing
readBusId n = Just $ read n

getBusTimes :: IO Buses
getBusTimes
  = do
    contents <- readFile "input"
    let ls = lines contents
        time = read $ head ls
        busIds = map readBusId $ splitOn "," $ last ls
    return $ Buses time busIds

earliestBus :: Buses -> Integer
earliestBus (Buses time buses)
  = let (t, b) = minimum $ map (\b -> (earliestTime b, b)) $ catMaybes buses
     in (t - time) * b
    where
      earliestTime :: Integer -> Integer
      earliestTime busId
        = ((time `div` busId) + 1) * busId

-- contest :: Buses -> Integer
-- contest (Buses _ xs')
--   = head (filter valid [x,2*x..]) - j
--     where
--       xs'' = catMaybes $ zipWith (\x' i -> fmap (,i) x') xs' [0..]
--       (x,j) = maximum xs''
--       xs = filter ((/= x) . fst) xs''
--       valid :: Integer -> Bool
--       valid y
--         = all (\(x', i) -> (y - j + i) `mod` x' == 0) xs

contest :: Buses -> Integer
contest (Buses _ xs')
  = let ((x,_):xs) = catMaybes $ zipWith (\x' i -> fmap (,i) x') xs' [0..]
     in fst $ foldl' contest' (x,x) xs

contest' :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
contest' (y,j) (x,i)
  = let j' = head $ filter (\j' -> (y + j' + i) `mod` x == 0) [j,2*j..]
     in (y + j', j * x)

main :: IO ()
main
  = do
    buses <- getBusTimes
    print $ earliestBus buses
    print $ contest buses
