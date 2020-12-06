import Data.Char (isAlpha)
import Data.List (nub)
import Data.List.Split (splitOn)

type Answers = String

getAnswers :: IO [Answers]
getAnswers
  = do
    contents <- readFile "input"
    return $ splitOn "\n\n" contents

countYeses :: Answers -> Int
countYeses
  = length . filter isAlpha . nub

countConsitentYeses :: Answers -> Int
countConsitentYeses answers
  = let questions = filter isAlpha $ nub answers
        answers' = words answers
     in length $ filter (\c -> all (c `elem`) answers') questions

main :: IO ()
main
  = do
    answers <- getAnswers
    print $ sum $ map countYeses answers
    print $ sum $ map countConsitentYeses answers
