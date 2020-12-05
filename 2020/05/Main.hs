import Data.Set (Set(..), fromList, member, notMember)

seatId :: [Char] -> Int
seatId []
  = 0
seatId (c:cs)
  | c `elem` "FL" = seatId cs
  | c `elem` "BR" = 2 ^ length cs + seatId cs

getSeatIds :: IO [Int]
getSeatIds
  = do
    contents <- readFile "input"
    return $ map seatId $ lines contents

findSeat :: [Int] -> Int
findSeat seatIds
  = let s = fromList seatIds
     in until (validSeat s) succ 0

validSeat :: Set Int -> Int -> Bool
validSeat s n
  = notMember n s && member (n - 1) s && member (n + 1) s

main :: IO ()
main
  = do
    seatIds <- getSeatIds
    print $ maximum seatIds
    print $ findSeat seatIds
