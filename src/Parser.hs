module Parser
  ( parseExpr
  ) where

import Syntax

import Data.Functor.Identity
import Text.Parsec
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok

-- | Specify the tokens for our language definition.
langDef :: Tok.LanguageDef ()
langDef =
  Tok.LanguageDef
  { Tok.commentStart = "{-"
  , Tok.commentEnd = "-}"
  , Tok.commentLine = "--"
  , Tok.nestedComments = True
  , Tok.identStart = letter
  , Tok.identLetter = alphaNum <|> oneOf "_'"
  , Tok.opStart = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Tok.opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Tok.reservedNames = []
  , Tok.reservedOpNames = []
  , Tok.caseSensitive = True
  }

-- | Create our lexical parser, defined by our language definition.
lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef

-- | Parse a given parser, enclosed in parentheses.
parens :: Parser a -> Parser a
parens = Tok.parens lexer

-- | Parse the given symbol name, while also checking its not a prefix of a
-- valid identifier.
reserved :: String -> Parser ()
reserved = Tok.reserved lexer

-- | Parses 0 or more occurences of the parser, separated by semicolons, returning
-- a list of values returned by the parser.
semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

-- | Parse the given symbol name, while also checking its not a prefix of a
-- valid operator.
reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

prefixOp :: String -> (a -> a) -> Ex.Operator String () Identity a
prefixOp s f = Ex.Prefix (reservedOp s >> return f)

-- | Table of prefix operators and their mapping to AST.
table :: Ex.OperatorTable String () Identity Expr
table = [[prefixOp "succ" Succ, prefixOp "pred" Pred, prefixOp "iszero" IsZero]]

-- | Parse if-then-else statements, getting each expression between the keywords.
ifthen :: Parser Expr
ifthen = do
  reserved "if"
  cond <- expr
  reservedOp "then"
  tr <- expr
  reserved "else"
  fl <- expr
  return (If cond tr fl)

-- | Constant True.
true :: Parser Expr
true = reserved "true" >> return Tr

-- | Constant False.
false :: Parser Expr
false = reserved "false" >> return Fl

-- | Constant Zero.
zero :: Parser Expr
zero = reserved "0" >> return Zero

-- | Generate the expression parser from the prefix operator table and from factor. 
expr :: Parser Expr
expr = Ex.buildExpressionParser table factor

-- | Our contains the terminals and a way back to expressions.
factor :: Parser Expr
factor = true <|> false <|> zero <|> ifthen <|> parens expr

-- | Parse the whole contents until the end of file.
contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

-- | The entrance of our parser, returning either the AST or an error.
parseExpr :: String -> Either ParseError Expr
parseExpr s = parse (contents expr) "<stdin>" s
