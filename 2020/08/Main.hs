data Instruction
  = JMP Int | ACC Int | NOP Int deriving (Show, Eq)

getArgument :: String -> Int
getArgument ('+':n) = read n
getArgument n = read n

getCommand :: String -> Int -> Instruction
getCommand "jmp" = JMP
getCommand "acc" = ACC
getCommand "nop" = NOP

getInstruction :: String -> Instruction
getInstruction text
  = let [cmd, n] = words text
     in getCommand cmd $ getArgument n

getProgram :: String -> IO [Instruction]
getProgram file
  = do
    contents <- readFile file
    return $ map getInstruction $ lines contents

run :: [Instruction] -> Int
run program
  = let (_, _, acc) = run' (program, [0], 0)
     in acc

run' :: ([Instruction], [Int], Int) -> ([Instruction], [Int], Int)
run' = until finished step

finished :: ([Instruction], [Int], Int) -> Bool
finished (program, pc:stack, _)
  = pc `elem` stack || pc >= length program || pc < 0

step :: ([Instruction], [Int], Int) -> ([Instruction], [Int], Int)
step (program, stack@(pc:_), acc)
  = case program !! pc of
      (JMP x) -> (program, (pc + x):stack, acc)
      (ACC x) -> (program, (pc + 1):stack, acc + x)
      (NOP _) -> (program, (pc + 1):stack, acc)

runAndFix :: [Instruction] -> Int
runAndFix program
  = let (_, _, acc) = runAndFix' (program, [0], 0)
     in acc

runAndFix' :: ([Instruction], [Int], Int) -> ([Instruction], [Int], Int)
runAndFix' state@(program, pc:stack, acc)
  | finished state = state
  | validTermination state' = state'
  | otherwise = run' (swapInstruction pc program, pc:stack, acc)
    where
      state' = runAndFix' $ step state

swapInstruction :: Int -> [Instruction] -> [Instruction]
swapInstruction i program
  = let (before, instr:after) = splitAt i program
     in case instr of
          (JMP x) -> before ++ NOP x : after
          (NOP x) -> before ++ JMP x : after
          _       -> program

validTermination :: ([Instruction], [Int], Int) -> Bool
validTermination (program, pc:_, _)
  = pc == length program

main :: IO ()
main
  = do
    program <- getProgram "input"
    print $ run program
    print $ runAndFix program
