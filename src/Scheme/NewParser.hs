{-# LANGUAGE OverloadedStrings #-}
module Scheme.NewParser where
import Data.Maybe
import Data.Text
import qualified Data.Text as T
import Data.Void
import Scheme.Ast
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Pos
import Text.Read (readMaybe)

type Parser = Parsec ScmParseError Text

bool :: Parser ScmAst
bool = do
  pos <- getSourcePos
  lexeme (symbol "#")
  boolValue <- identifier
  case boolValue of
    "t" -> return $ Bool pos True
    "f" -> return $ Bool pos False
    a -> customFailure (Message "invalid bool")

atom :: Parser ScmAst
atom = do
  pos <- getSourcePos
  id <- identifier
  return $ convert pos id
  where
    convert :: SourcePos -> Text -> ScmAst
    convert pos atom
      | isNumber atom = Number pos (read (T.unpack atom))
      | otherwise = Atom pos atom
    isNumber s = isJust (readMaybe (T.unpack s) :: Maybe Integer)

identifier :: Parser Text
identifier = T.pack <$> lexeme (some alphaNumChar) <?> "identifier"
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc
symbol :: Text -> Parser Text
symbol w = L.symbol sc w <?> T.unpack w
keyword :: Text -> Parser Text
keyword w = L.symbol sc w <?> T.unpack w

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
