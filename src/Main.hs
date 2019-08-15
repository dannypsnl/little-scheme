module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad (liftM)

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn (readExpr expr)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "no match: " ++ show err
  Right val -> "found value"

parseExpr :: Parser ScmValue
parseExpr = parseAtom
  <|> parseString
  <|> parseNumber
  <|> parseQuoted
  <|> do char '('
         x <- try parseList <|> parsePair
         char ')'
         return x

spaces :: Parser ()
spaces = skipMany1 space

data ScmValue =
  Atom String
  | List [ScmValue]
  | Pair [ScmValue] ScmValue
  | Number Integer
  | String String
  | Bool Bool


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
parseList = liftM List $ sepBy parseExpr spaces

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
