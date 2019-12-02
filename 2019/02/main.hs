seperate :: String -> [String]
seperate ""
  = []
seperate text
  = word : seperate remaining
    where
      word = takeWhile (/= ',') text
      remaining = drop (succ $ length word) text

getCodes :: IO [Int]
getCodes
  = do
    input <- readFile "input"
    return $ map read $ seperate input

replace :: Int -> Int -> [Int] -> [Int]
replace pos value values
  = before ++ (value : after)
    where
      before = take pos values
      after  = drop (succ pos) values

update :: Int -> Int -> Int -> Int -> [Int] -> [Int]
update opcode in1 in2 out codes
  = replace out (in1 `operation` in2) codes
    where
      operation
        | opcode == 1 = (+)
        | otherwise   = (*)

process :: [Int] -> [Int]
process
  = process' 0
    where
      process' :: Int -> [Int] -> [Int]
      process' pos codes
        | codes !! pos == 99 = codes
        | otherwise          = process' (pos + 4) codes'
          where
            opcode = codes !! pos
            in1    = codes !! (codes !! (pos + 1))
            in2    = codes !! (codes !! (pos + 2))
            out    = codes !! (pos + 3)
            codes' = update opcode in1 in2 out codes

run :: Int -> Int -> [Int] -> Int
run noun verb (c : _ : _ : codes)
  = head $ process (c : noun : verb : codes)

part1 :: IO Int
part1
  = do
    codes <- getCodes
    return $ run 12 2 codes
    
part2 :: IO (Int, Int)
part2
  = do
    codes <- getCodes
    return $ head [(x, y) | x <- [0..99], y <- [0..9], (run x y codes) == 19690720]
