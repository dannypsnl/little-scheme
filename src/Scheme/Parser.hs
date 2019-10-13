module Scheme.Parser (
  readExpr,
  readExprList
) where
import Scheme.Core (ScmError(ParserErr), ScmValue(..), ThrowsError)

import Control.Monad.Except (throwError)
import Data.Functor.Identity (Identity)
import Data.Maybe (isJust)
import Text.Parsec (between, many, parse, try, (<|>))
import Text.Parsec.Char (digit, letter, noneOf, oneOf)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Prim (ParsecT, unexpected)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Token
import Text.Read (readMaybe)

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
  parseBool
  <|> parseAtom
  <|> parseString
  <|> parseNumber
  <|> (try parseNegNum <|> parseQuoted)
  <|> (try (parseList parens) <|> try (parseList brackets) <|> try (parsePair "()") <|> parsePair "[]")

parseBool :: Parser ScmValue
parseBool = do
  lexeme (reservedOp "#")
  boolValue <- identifier
  case boolValue of
    "t" -> return $ Bool True
    "f" -> return $ Bool False
    a -> unexpected a

parseAtom :: Parser ScmValue
parseAtom = convert <$> identifier
  where
    convert atom
      | isNumber atom = Number (read atom)
      | otherwise = Atom atom
    isNumber s = isJust (readMaybe s :: Maybe Integer)

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

parseList :: Functor f => (ParsecT String () Identity [ScmValue] -> f [ScmValue]) -> f ScmValue
parseList wrapper = List <$> wrapper (many parseExpr)

parsePair :: String -> Parser ScmValue
parsePair [leftWrapper, rightWrapper] = do
  pHead <- pairHead (many parseExpr)
  pTail <- parseExpr
  reservedOp [rightWrapper]
  return $ Pair pHead pTail
  where
    pairHead = between (reservedOp [leftWrapper]) (reservedOp ".")
parsePair badPairWrapper = unexpected ("Expected () or [] but got: " ++ badPairWrapper)

parseQuoted :: Parser ScmValue
parseQuoted = do
  reservedOp "'"
  x <- parseExpr
  return $ List [Atom "quote", x]

lexeme :: ParsecT String () Identity a -> ParsecT String () Identity a
lexeme = Token.lexeme lexer
identifier :: ParsecT String () Identity String
identifier = Token.identifier lexer
integer :: ParsecT String () Identity Integer
integer = Token.integer lexer
reservedOp :: String -> ParsecT String () Identity ()
reservedOp = Token.reservedOp lexer
parens :: ParsecT String () Identity a -> ParsecT String () Identity a
parens = Token.parens lexer
brackets :: ParsecT String () Identity a -> ParsecT String () Identity a
brackets = Token.brackets lexer

lexer :: Token.GenTokenParser String () Identity
lexer = Token.makeTokenParser languageDef

-- reference: https://schemers.org/Documents/Standards/R5RS/r5rs.pdf
-- page 5, section 2.1 Identifiers
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

languageDef :: Token.GenLanguageDef String () Identity
languageDef = emptyDef {
  Token.commentStart = "#|"
  , Token.commentEnd = "|#"
  , Token.commentLine = ";"
  , Token.identStart = letter <|> symbol
  , Token.identLetter = digit <|> letter <|> symbol
  , Token.reservedOpNames = [ "'", "\"", ".", "-" ]
  }
