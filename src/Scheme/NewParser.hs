{-# LANGUAGE OverloadedStrings #-}
module Scheme.NewParser (
  expr
  , ScmParseError(..)
) where
import Data.Maybe
import Data.Text
import qualified Data.Text as T
import Data.Void
import Scheme.Ast.Stage0
import Text.Megaparsec
import Text.Megaparsec.Char hiding (string)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Pos
import Text.Read (readMaybe)

type Parser = Parsec ScmParseError Text

expr :: Parser Stage0
expr = quoted
  <|> bool
  <|> atom
  <|> string
  <|> sexpr

sexpr :: Parser Stage0
sexpr = do
  pos <- getSourcePos
  es <- parens (many expr)
  return $ List pos es

quoted :: Parser Stage0
quoted = do
  pos <- getSourcePos
  symbol "'"
  x <- expr
  return $ Quoted pos x

bool :: Parser Stage0
bool = do
  pos <- getSourcePos
  lexeme (symbol "#")
  boolValue <- identifier
  case boolValue of
    "t" -> return $ Bool pos True
    "f" -> return $ Bool pos False
    a -> customFailure (Message "invalid bool")

atom :: Parser Stage0
atom = do
  pos <- getSourcePos
  id <- identifier
  return $ convert pos id
  where
    convert :: SourcePos -> Text -> Stage0
    convert pos atom
      | isNumber atom = Number pos (read (T.unpack atom))
      | otherwise = Atom pos atom
    isNumber s = isJust (readMaybe (T.unpack s) :: Maybe Integer)

string :: Parser Stage0
string = do
  pos <- getSourcePos
  symbol "\""
  p <- many $ noneOf ("\"" :: String)
  symbol "\""
  return $ String pos (T.pack p)

identifier :: Parser Text
identifier = T.pack <$> lexeme (some alphaNumChar) <?> "identifier"
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc
symbol :: Text -> Parser Text
symbol w = L.symbol sc w <?> T.unpack w
keyword :: Text -> Parser Text
keyword w = L.symbol sc w <?> T.unpack w
parens = between (symbol "(") (symbol ")")
brackets = between (symbol "[") (symbol "]")

integer :: Parser Integer
integer = L.signed sc (lexeme L.decimal)

-- sc
--
-- space consumer
sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment ";")
  (L.skipBlockComment "#|" "#|")

-- Errors
data ScmParseError = Message Text
  deriving (Eq, Show, Ord)

instance ShowErrorComponent ScmParseError where
  showErrorComponent (Message t) = T.unpack t
