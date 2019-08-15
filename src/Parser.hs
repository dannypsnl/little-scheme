module Parser (
  ScmValue(..),
  parseExpr
) where
import           Control.Monad                 (liftM)
import           Text.ParserCombinators.Parsec hiding (spaces)

data ScmValue =
    Atom String
    | List [ScmValue]
    | Pair [ScmValue] ScmValue
    | Number Integer
    | String String
    | Bool Bool

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

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
    _    -> Atom atom

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

instance Show ScmValue where
  show = showValue

showValue :: ScmValue -> String
showValue (String str) = "\"" ++ str ++ "\""
showValue (Atom name) = name
showValue (Number num) = show num
showValue (Bool True) = "#t"
showValue (Bool False) = "#f"
showValue (List contents) = "(" ++ unwordsList contents ++ ")"
showValue (Pair head tail) = "(" ++ unwordsList head ++ " . " ++ showValue tail ++ ")"

unwordsList :: [ScmValue] -> String
unwordsList = unwords . map showValue
