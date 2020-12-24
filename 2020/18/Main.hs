import Data.Char (isDigit)

data Expr
  = Number Int
  | BinOp Op Expr Expr
    deriving Show

data Token
  = TokNum Int
  | TokOp Op
  | TokLParen
  | TokRParen
    deriving (Show,Eq)

data Op = Mul | Add deriving (Show, Eq, Ord)

parseExpr :: String -> Expr
parseExpr = parse . tokenize

tokenize :: String -> [Token]
tokenize ""       = []
tokenize (' ':cs) = tokenize cs
tokenize ('+':cs) = TokOp Add : tokenize cs
tokenize ('*':cs) = TokOp Mul : tokenize cs
tokenize ('(':cs) = TokLParen : tokenize cs
tokenize (')':cs) = TokRParen : tokenize cs
tokenize cs       = let (num, cs') = span isDigit cs
                     in TokNum (read num) : tokenize cs'

parse :: [Token] -> Expr
parse = parse' [] . shuntingYard

parse' :: [Expr] -> [Token] -> Expr
parse' [e] [] = e
parse' (rhs:lhs:stack) ((TokOp op):ts) = parse' (BinOp op lhs rhs : stack) ts
parse' stack ((TokNum n):ts) = parse' (Number n : stack) ts
parse' _ _ = error "error with parse"

shuntingYard :: [Token] -> [Token]
shuntingYard = shuntingYard' []

shuntingYard' :: [Token] -> [Token] -> [Token]
shuntingYard' stack [] = stack
shuntingYard' stack (t:ts)
  = case t of
      (TokNum _) -> t : shuntingYard' stack ts
      TokOp op   -> let (ts', stack') = popStack op stack
                     in ts' ++ shuntingYard' (t:stack') ts
      TokLParen  -> shuntingYard' (t:stack) ts
      TokRParen  -> let (ts', _:stack') = break (== TokLParen) stack
                     in ts' ++ shuntingYard' stack' ts

popStack :: Op -> [Token] -> ([Token], [Token])
popStack op
  = span shouldPop
    where
      shouldPop :: Token -> Bool
      shouldPop TokLParen = False
      shouldPop (TokOp op') = op' >= op

getExprs :: IO [Expr]
getExprs = map (parse . tokenize) . lines <$> readFile "input"

eval :: Expr -> Int
eval (Number x)    = x
eval (BinOp Add lhs rhs) = eval lhs + eval rhs
eval (BinOp Mul lhs rhs) = eval lhs * eval rhs

main :: IO ()
main
  = do
    exprs <- getExprs
    print $ sum $ map eval exprs
