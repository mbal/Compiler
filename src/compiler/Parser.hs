module Parser where
import Control.Monad
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Control.Applicative ((<*))
import qualified Text.ParserCombinators.Parsec.Token as Token

data BOperation = Add
                | Subtract
                | Multiply
                | Divide
                | At
                deriving (Show, Eq)

data UOperation = Minus
                | Fix
                deriving (Show, Eq)

data Term = Var Name -- variable definition
          | Lambda Name Term -- function
          | FunApp Term [Term] -- function application
          | Const Value -- constant definition
          | Array [Term]
          | BinaryOp BOperation Term Term -- binary operation
          | UnaryOp UOperation Term -- unary operation (unary -, fix operator)
          | If Term Term Term -- if
          | Let Name Term -- let binding
          deriving (Show, Eq)

type Name = String
type Value = Integer -- currently, we support only integers

languageDef =
  emptyDef { Token.commentStart    = "/*"
           , Token.commentEnd      = "*/"
           , Token.commentLine     = "//"
           , Token.identStart      = letter
           , Token.identLetter     = alphaNum
           , Token.reservedNames   = [ "if"
                                     , "then"
                                     , "else"
                                     , "true"
                                     , "false"
                                     , "not"
                                     , "and"
                                     , "or"]
           , Token.reservedOpNames = [ "+", "-", "*", "/", "=", "@"
                                     , "and", "or", "not" ]
           }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis:
                                    --   parens p
                                    -- takes care of the parenthesis and
                                    -- uses p to parse what's inside them
integer    = Token.integer    lexer -- parses an integer
semi       = Token.semi       lexer -- parses a semicolon
whiteSpace = Token.whiteSpace lexer -- parses whitespace

brackets = Token.brackets lexer
comma = Token.comma lexer

array =
  do c <- brackets (sepBy expression comma) -- parses an array
     return $ Array c


parser = seqOfExpression <* eof

seqOfExpression =
  do list <- (endBy expr' semi)
     return list

expression = expr'
expr' = letBinding <|> ifExpression <|> arithExpression <|> array

ifExpression =
  do reserved "if"
     cond <- expression
     reserved "then"
     thenBranch <- expression
     reserved "else"
     elseBranch <- expression
     return $ If cond thenBranch elseBranch

letBinding =
  do reserved "let"
     name <- identifier
     reserved "="
     value <- expression
     return $ Let name value

term = parens expression
     <|> liftM Var identifier
     <|> liftM Const integer
     <|> array

funApplication =
  try (do
          fun <- identifier
          a <- parens (sepBy expression comma)
          return $ FunApp (Var fun) a)
  <|> delimExpr

delimExpr = parens expression
            <|> liftM Var identifier
            <|> liftM Const integer
            <|> array

arithExpression = buildExpressionParser aOperators funApplication

aOperators = [ [Prefix (reservedOp "-" >> return (UnaryOp Minus)) ]
             , [Infix (reservedOp "@" >> return (BinaryOp At)) AssocLeft]
             , [Infix (reservedOp "*" >> return (BinaryOp Multiply)) AssocLeft]
             , [Infix (reservedOp "/" >> return (BinaryOp Divide)) AssocLeft]
             , [Infix (reservedOp "+" >> return (BinaryOp Add)) AssocLeft]
             , [Infix (reservedOp "-" >> return (BinaryOp Subtract)) AssocLeft]
              ]

-- function for testing
parseFile file =
  do program  <- readFile file
     case parse parser "" program of
       Left e  -> print e >> fail "parse error"
       Right r -> return r

parseString str =
  case parse parser "" str of
    Left e -> error $ show e
    Right r -> r

parseAST = parseString
