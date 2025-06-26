{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module IMP where

import Control.Monad.Trans.State
import Parser

newtype Program = Program [Stmt]
                  deriving (Show, Eq)

data Stmt = Assign String Expr
          | If Expr Program Program
          | While Expr Program
          deriving (Show, Eq)

data Expr = Lit Int
          | Var String
          | Bin String Expr Expr
          deriving (Show, Eq)


-- PARSER --

program :: Parser Program
program = do
  stmts <- oneOrMore (stmt <* symbol ";")
  return (Program stmts)

stmt :: Parser Stmt
stmt = ifStmt <|> whileStmt <|> assign

assign :: Parser Stmt
assign = do
  id <- identifier
  symbol "="
  e <- expr
  return (Assign id e)

ifStmt :: Parser Stmt
ifStmt = do
  symbol "if"
  cond <- expr
  symbol "then"
  p1 <- program
  symbol "else"
  p2 <- program
  symbol "end"
  return (If cond p1 p2)

whileStmt :: Parser Stmt
whileStmt = do
  symbol "while"
  cond <- expr
  symbol "do"
  body <- program
  symbol "end"
  return (While cond body)

expr :: Parser Expr
expr = additive

additive :: Parser Expr
additive = do
  t <- multiplicative
  rest t
  where
    rest left = (do op <- oneOf ["+", "-"]
                    right <- additive
                    rest (Bin op left right)) <|> return left

multiplicative :: Parser Expr
multiplicative = do
  t <- relational
  rest t
  where
    rest left = (do op <- oneOf ["*", "/"]
                    right <- multiplicative
                    rest (Bin op left right)) <|> return left

relational :: Parser Expr
relational = do
  t <- equality
  rest t
  where
    rest left = (do op <- oneOf ["<", ">"]
                    right <- relational
                    rest (Bin op left right)) <|> return left

equality :: Parser Expr
equality = do
  t <- term
  rest t
  where
    rest left = (do op <- oneOf ["==", "!="]
                    right <- equality
                    rest (Bin op left right)) <|> return left

term :: Parser Expr
term =  Var <$> identifier 
    <|> Lit <$> int


type Env = [(String, Int)]

eval :: Program -> Env -> Env
eval (Program stmts) env = foldl evalStmt env stmts

evalStmt :: Env -> Stmt -> Env
evalStmt env (Assign id e) = 
    let value = evalExpr e env
        filteredEnv = filter ((/= id) . fst) env
    in (id, value) : filteredEnv
evalStmt env (If cond thenBranch elseBranch) =
    if evalExpr cond env /= 0
        then eval thenBranch env
        else eval elseBranch env
evalStmt env (While cond body) =
    if evalExpr cond env /= 0
        then evalStmt (eval body env) (While cond body)
        else env

evalExpr :: Expr -> Env -> Int
evalExpr (Lit n) _ = n
evalExpr (Var x) env = case lookup x env of
    Just v  -> v
    Nothing -> 0  -- If variable is not found, treat it as 0
evalExpr (Bin op e1 e2) env =
    let v1 = evalExpr e1 env
        v2 = evalExpr e2 env
    in case op of
        "+"  -> v1 + v2
        "*"  -> v1 * v2
        "<"  -> if v1 < v2 then 1 else 0
        ">"  -> if v1 > v2 then 1 else 0
        "==" -> if v1 == v2 then 1 else 0
        "!=" -> if v1 /= v2 then 1 else 0
        _    -> error "Unsupported operator"

parseEval :: String -> Env -> Either String Env
parseEval s env = case parse program s of
  Left err -> Left (show err)
  Right (p, _) -> Right (eval p env)


