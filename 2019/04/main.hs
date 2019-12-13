decreasing :: String -> Bool
decreasing text
  = and $ zipWith (<=) (init text) (tail text)

containsDouble :: String -> Bool
containsDouble text
  = or $ zipWith (==) (init text) (tail text)

containsDoubleStrict :: String -> Bool
containsDoubleStrict ""
  = False
containsDoubleStrict (c : cs)
  = length letters == 1 || containsDoubleStrict remaining
    where
      letters   = takeWhile (== c) cs
      remaining = dropWhile (== c) cs

isValid :: String -> Bool
isValid password
  = decreasing password && containsDouble password

isValidStrict :: String -> Bool
isValidStrict password
  = decreasing password && containsDoubleStrict password

count :: (String -> Bool) -> Int
count predicate
  = length $ filter predicate $ map show [172930 .. 683082]

part1 :: Int
part1
  = count isValid

part2 :: Int
part2
  = count isValidStrict
