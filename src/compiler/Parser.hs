module Parser where

import Text.ParserCombinators.Parsec hiding (State)
import Control.Monad.Reader
import Debug.Trace
import Control.Monad.State
import Control.Applicative hiding ((<|>), many)

program = parseSum

data Expr = Sum Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Num Integer
          deriving (Show)

parseSum = chainl1 expr plusMinusOp

plusMinusOp :: Parser (Expr -> Expr -> Expr)
plusMinusOp = do
  many space
  l <- (char '+') <|> (char '-')
  many space
  case l of
    '+' -> return Sum
    '-' -> return Sub

expr = (chainl1 number mulDivOp) <|> number
mulDivOp = do
  many space
  l <- (char '*') <|> (char '/')
  many space
  case l of
    '*' -> return Mul
    '/' -> return Div

number = try (do
  n <- many1 digit
  return $ Num (read n :: Integer)) <|>
         (do
             char '('
             s <- parseSum
             char ')'
             return s)

parseAST = run

run :: String -> Expr
run prog =
  case (parse program "" prog) of
    Left err -> error (show err)
    Right x -> x
  
