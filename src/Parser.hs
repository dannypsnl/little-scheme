module Parser (
  parseExpr,
  readExpr,
  readExprList
) where
import Core (ScmError(ParserErr), ScmValue(..), ThrowsError)

import Control.Monad.Except (throwError)
import Text.Parsec (between, endBy, many, many1, noneOf, sepBy, skipMany1, space, try, (<|>))
import Text.Parsec (parse)
import Text.Parsec.Char (char, digit, letter, oneOf)
import Text.Parsec.String (Parser)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

parseExpr :: Parser ScmValue
parseExpr = parseAtom
  <|> parseString
  <|> parseNumber
  <|> parseQuoted
  <|> parens (try parseList <|> parsePair)

-- between takes open and close paresr and returns a Parser to Parser function at here
parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser ScmValue
parseString = do
  char '"'
  x <- many (noneOf "\"")
  char '"'
  return $ String x

parseAtom :: Parser ScmValue
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _ -> Atom atom

parseNumber :: Parser ScmValue
parseNumber = do
  num <- many1 digit
  return $ Number (read num :: Integer)

parseList :: Parser ScmValue
parseList = List <$> sepBy parseExpr spaces

parsePair :: Parser ScmValue
parsePair = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ Pair head tail

parseQuoted :: Parser ScmValue
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "scheme" input of
  Left err -> throwError $ ParserErr err
  Right val -> return val

readExpr = readOrThrow parseExpr
readExprList = readOrThrow (endBy parseExpr spaces)
