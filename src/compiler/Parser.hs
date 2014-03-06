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
                | Compose  
                | Greater
                | Lesser
                | Equal  
                | NotEqual  
                | GEQ
                | LEQ
                | And
                | Or
                deriving (Show, Eq)

data UOperation = Minus
                | Plus
                | Prime
                | Not
                deriving (Show, Eq)

data Term = Var Identifier -- variable definition
          | FunApp Term [Term] -- function application
          | Const Value -- constant definition
          | Array [Term]
          | BinaryOp BOperation Term Term -- binary operation
          | UnaryOp UOperation Term -- unary operation (unary -, prime operator)
          | If Term Term Term -- if
          | Let Identifier Term -- let binding
          | Defun Identifier [Identifier] Term
          | Hook Term Term  
          | BinOp BOperation
          | UnOp UOperation  
          deriving (Show, Eq)

type Identifier = String

data Value = Number Integer
           | Nil
           | VTrue
           | VFalse
           | StringValue String
           deriving (Eq, Show)  

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
                                     , "nil"]
           , Token.reservedOpNames = [ "+", "-", "'", "*", "/", "=", "@"
                                     , "<", ">", "<=", ">=", "==", "."
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
stringLiteral     = Token.stringLiteral lexer

brackets = Token.brackets lexer
comma = Token.comma lexer

nil = do reserved "none"
         return $ Const Nil
true = do reserved "true"
          return $ Const VTrue
false = do reserved "false"
           return $ Const VFalse

number = do v <- integer
            return $ Const (Number v)

stringValue = do
  value <- stringLiteral
  return $ Const (StringValue value)

array =
  do c <- brackets (sepBy expr' comma) -- parses an array
     return $ Array c

parser = (many ((letBinding <|> expression) <* semi)) <* eof

seqOfExpression =
  do list <- (endBy expr' semi)
     return list

expression = expr'
expr' = ifExpression
        <|> functionDefinition
        <|> arithExpression

constant = nil
           <|> true
           <|> false
           <|> stringValue
           <|> number

functionDefinition =
  do reserved "function"
     name <- identifier
     parameters <- parens formalParams
     reserved "="
     body <- expression
     return $ Defun name parameters body

variable = do name <- identifier
              return $ Var name
           
formalParams = sepBy identifier comma
  
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

funApplication =
  try (do fun <- delimExpr
          a <- parens (sepBy expression comma)
          return $ FunApp fun a)
  <|> delimExpr

delimExpr = (try (parens hook))
            <|> (try funCompose)
            <|> parens expression
            <|> variable
            <|> constant
            <|> array

funCompose = chainr1 opOrFun (do { (reserved "."); return $ BinaryOp Compose })

hook = do
  f1 <- funCompose
  f2 <- funCompose
  return (Hook f1 f2)

opOrFun = variable
          <|> ((reserved "+") >> return (BinOp Add))
          <|> ((reserved "*") >> return (BinOp Multiply))

arithExpression = buildExpressionParser aOperators funApplication

aOperators = [ [ prefix "-" (UnaryOp Minus) ]
             , [ prefix "+" (UnaryOp Plus) ]
             , [ prefix "not" (UnaryOp Not) ]
             , [ postfix "'" (UnaryOp Prime) ]
             , [ binary "@" (BinaryOp At) AssocLeft ]
             , [ binary "*" (BinaryOp Multiply) AssocLeft,
                 binary "/" (BinaryOp Divide) AssocLeft ]
             , [ binary "+" (BinaryOp Add) AssocLeft,
                 binary "-" (BinaryOp Subtract) AssocLeft ]
             , [ binary ">" (BinaryOp Greater) AssocNone,
                 binary "<" (BinaryOp Lesser) AssocNone,
                 binary "<=" (BinaryOp LEQ) AssocNone,
                 binary ">=" (BinaryOp GEQ) AssocNone,
                 binary "==" (BinaryOp Equal) AssocNone,
                 binary "!=" (BinaryOp NotEqual) AssocNone ]
             , [ binary "and" (BinaryOp And) AssocLeft,
                 binary "or" (BinaryOp Or) AssocLeft ]
             ]

prefix name fun = Prefix (do { reservedOp name; return fun })
postfix name fun = Postfix (do {reservedOp name; return fun })
binary name fun assoc = Infix (do { reservedOp name; return fun }) assoc
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
