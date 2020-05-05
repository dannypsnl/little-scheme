{-# LANGUAGE OverloadedStrings #-}
module NewParserSpec where
import SpecHelper
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Text.Megaparsec.Pos

import Scheme.Ast.Stage0
import Scheme.NewParser

spec :: Spec
spec = describe "parse" $ do
  context "stage0" $ do
    it "atom" $ do
      parse expr "" "aaa" `shouldParse` (Atom (mkSPos 1 1) "aaa")
    it "string" $ do
      parse expr "" "\"aaa\"" `shouldParse` (String (mkSPos 1 1) "aaa")
    it "number" $ do
      parse expr "" "123" `shouldParse` (Number (mkSPos 1 1) 123)
    it "bool true" $ do
      parse expr "" "#t" `shouldParse` (Bool (mkSPos 1 1) True)
    it "bool false" $ do
      parse expr "" "#f" `shouldParse` (Bool (mkSPos 1 1) False)
    it "quoted S expr" $ do
      parse expr "" "'()" `shouldParse` (Quoted (mkSPos 1 1) (List (mkSPos 1 2) []))
    it "S expr" $ do
      parse expr "" "(lambda (e) e)" `shouldParse` (List (mkSPos 1 1) [Atom (mkSPos 1 2) "lambda", List (mkSPos 1 9) [Atom (mkSPos 1 10) "e"], Atom (mkSPos 1 13) "e"])

mkSPos :: Int -> Int -> SourcePos
mkSPos l c = (SourcePos "" (mkPos l) (mkPos c))
