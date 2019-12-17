type Body
  = String
type Orbit
  = (Body, Body) -- second orbits the first

getOrbits :: IO [Orbit]
getOrbits
  = do
    input <- readFile "input"
    return $ map parseOrbit $ lines input
      where
        parseOrbit :: String -> Orbit
        parseOrbit text
          = (take 3 text, drop 4 text)

findOrbiting :: [Orbit] -> Body -> Body
findOrbiting [] body
  = ""
findOrbiting ((center, satellite) : orbits) body
  | satellite == body = center
  | otherwise         = findOrbiting orbits body

findOrbitedBy :: Body -> [Orbit] -> [Body]
findOrbitedBy body
  = map snd . filter ((==) body . fst)

uniqueBodies :: [Orbit] -> [Body]
uniqueBodies
  = uniqueBodies' []
    where
      uniqueBodies' :: [Body] -> [Orbit] -> [Body]
      uniqueBodies' bodies []
        = bodies
      uniqueBodies' bodies ((center, satellite) : orbits)
        = uniqueBodies' (append center $ append satellite bodies) orbits
      append :: Body -> [Body] -> [Body]
      append body bodies
        | filter (== body) bodies == [] = body : bodies
        | otherwise                     = bodies

numberOfOrbits :: [Orbit] -> Body -> Int
numberOfOrbits orbits body
  = (length orbitedBy) + (sum $ map (numberOfOrbits orbits) orbitedBy)
    where
      orbitedBy = findOrbitedBy body orbits

findOrbitChain :: [Orbit] -> Body -> [Body]
findOrbitChain orbits body
  = reverse $ takeWhile (/= "") $ iterate (findOrbiting orbits) body

-- SLOW
-- could be improved by not having to perform repeat calculations
part1 :: IO Int
part1
  = do
    orbits <- getOrbits
    let bodies = uniqueBodies orbits
    return $ sum $ map (numberOfOrbits orbits) bodies

part2 :: IO Int
part2
  = do
    orbits <- getOrbits
    let chainYOU = findOrbitChain orbits "YOU"
        chainSAN = findOrbitChain orbits "SAN"
        overlap  = length $ takeWhile id $ zipWith (==) chainYOU chainSAN
    return $ (length chainYOU) + (length chainSAN) - 2 * overlap - 2
