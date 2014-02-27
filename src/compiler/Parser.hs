module Parser where

import Text.ParserCombinators.Parsec hiding (State)
import Control.Monad.Reader
import Debug.Trace
import Control.Monad.State
import Control.Applicative hiding ((<|>), many, optional)

program = parseSum

data Expr = Sum Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Num Integer
          | FunctionCall String [Expr]
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

number =
  try (do n <- many1 digit
          return $ Num (read n :: Integer))
  <|> try (do char '('
              s <- parseSum
              char ')'
              return s)
  <|> (do fname <- identifier
          char ' '
          args <- argumentList
          return (FunctionCall fname args))

identifier = many1 letter

argumentList =
  try (do arg <- argument
          args <- optionMaybe (do char ','
                                  char ' '
                                  args <- argumentList
                                  return args)
          case args of
            Just x -> return $ arg : x
            Nothing -> return $ [arg])
  <|> (return [])

argument = parseSum
{-  
argumentList =
  try (do arg1 <- parseSum
          args <- (do char ','
                      char ' '
                      arg <- parseSum
                      rest <- argumentList
                      return $ arg : rest)
          return $ arg1 : args )
  <|> (do arg1 <- parseSum
          return [arg1])
  <|> (return []) -}

parseAST = run

run :: String -> Expr
run prog =
  case (parse program "" prog) of
    Left err -> error (show err)
    Right x -> x
  
