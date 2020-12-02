import Data.Text (pack, unpack)
import Data.Attoparsec.Text
import Data.Char (isPunctuation)

newtype Range
  = Range (Int, Int)
    deriving Show

data Password
  = Password Range Char String
    deriving Show

range :: Parser Range
range
  = do
    lower <- decimal
    skipMany1 (char '-')
    upper <- decimal
    return $ Range (lower, upper)

password :: Parser Password
password
  = do
    r <- range
    skipSpace
    c <- anyChar
    skip isPunctuation
    skipSpace
    Password r c . unpack <$> takeText

getPassword :: String -> Password
getPassword line
  = case parseOnly password $ pack line of
      Left e -> error e
      Right p -> p

getPasswords :: IO [Password]
getPasswords
  = do
    content <- readFile "input"
    return $ map getPassword $ lines content

isValid :: Password -> Bool
isValid (Password (Range (lower, upper)) c pass)
  = let number = length $ filter (== c) pass
     in lower <= number && number <= upper

xor :: Bool -> Bool -> Bool
xor lhs rhs
  = (lhs || rhs) && not (lhs && rhs)

isValid' :: Password -> Bool
isValid' (Password (Range (first, second)) c pass)
  = (pass !! (first - 1) == c) `xor` (pass !! (second - 1) == c)

main :: IO ()
main
  = do
    passwords <- getPasswords
    print $ length $ filter isValid passwords
    print $ length $ filter isValid' passwords
