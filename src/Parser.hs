module Parser (
  readExpr,
  readExprList
) where
import Core (ScmError(ParserErr), ScmValue(..), ThrowsError)

import Control.Monad.Except (throwError)
import Text.Parsec (between, endBy, many, many1, manyTill, optionMaybe, parse, sepBy, try, (<|>))
import Text.Parsec.Char (char, digit, letter, newline, noneOf, oneOf, space)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Token

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "scheme" input of
  Left err -> throwError $ ParserErr err
  Right val -> return val

readExpr :: String -> ThrowsError ScmValue
readExpr = readOrThrow parseExpr

readExprList :: String -> ThrowsError [ScmValue]
readExprList = readOrThrow (many parseExpr)

parseExpr :: Parser ScmValue
parseExpr =
  parseAtom
  <|> parseString
  <|> parseNumber
  <|> (try parseNegNum <|> parseQuoted)
  <|> (try parseList <|> parsePair)

parseAtom :: Parser ScmValue
parseAtom = do
  atom <- identifier
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _ -> Atom atom

parseString :: Parser ScmValue
parseString = do
  reservedOp "\""
  p <- many $ noneOf "\""
  reservedOp "\""
  return $ String p

parseNumber :: Parser ScmValue
parseNumber = Number <$> integer

parseNegNum :: Parser ScmValue
parseNegNum = do
  reservedOp "-"
  d <- integer
  return $ Number . negate $ d

parseList = List <$> parens (many parseExpr)

parsePair :: Parser ScmValue
parsePair = do
  head <- pairHead (many parseExpr)
  tail <- parseExpr
  reservedOp ")"
  return $ Pair head tail
  where
    pairHead = between (reservedOp "(") (reservedOp ".")

parseQuoted :: Parser ScmValue
parseQuoted = do
  reservedOp "'"
  x <- parseExpr
  return $ List [Atom "quote", x]

identifier = Token.identifier lexer
integer = Token.integer lexer
reservedOp = Token.reservedOp lexer
parens = Token.parens lexer
whiteSpace = Token.whiteSpace lexer

lexer = Token.makeTokenParser languageDef

-- reference: https://schemers.org/Documents/Standards/R5RS/r5rs.pdf
-- page 5, section 2.1 Identifiers
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

languageDef = emptyDef {
  Token.commentStart = "#|"
  , Token.commentEnd = "|#"
  , Token.commentLine = ";"
  , Token.identStart = letter <|> symbol
  , Token.identLetter = digit <|> letter <|> symbol
  , Token.reservedOpNames = [ "'", "\"", ".", "-" ]
  }
