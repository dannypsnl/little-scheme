{-# LANGUAGE OverloadedStrings #-}
module Ast.Stage1Spec where
import Control.Monad.Except (runExceptT)
import SpecHelper
import Text.Megaparsec.Pos

import Scheme.Ast.Stage0
import Scheme.Ast.Stage1

spec :: Spec
spec = describe "transform" $ do
  context "stage0 -> stage1" $ do
    it "transform reuse part" $ do
      (Atom pos "aaa") `transResultIs` (Stage0 (Atom pos "aaa"))
    it "lambda form" $ do
      (List pos [Atom pos "lambda", List pos [], Number pos 1]) `transResultIs`
        (Lambda pos [] [(Stage0 (Number pos 1))])
    it "define variable form" $ do
      (List pos [Atom pos "define", Atom pos "a", Number pos 1]) `transResultIs`
        (Define pos "a" Nothing [(Stage0 (Number pos 1))])
    it "define function form" $ do
      (List pos [Atom pos "define", (List pos [Atom pos "a", Atom pos "b"]), Atom pos "b"]) `transResultIs`
        (Define pos "a" (Just ["b"]) [(Stage0 (Atom pos "b"))])
    it "define function form without parameters" $ do
      (List pos [Atom pos "define", (List pos [Atom pos "a"]), Atom pos "b"]) `transResultIs`
        (Define pos "a" (Just []) [(Stage0 (Atom pos "b"))])
    it "let form" $ do
      (List pos [Atom pos "let", (List pos [List pos [Atom pos "a", Number pos 1]]), Atom pos "a"]) `transResultIs`
        (Let pos [Binding pos "a" (Stage0 (Number pos 1))] [(Stage0 (Atom pos "a"))])
  where
    transResultIs stage0 expectedStage1 = (runExceptT $ fromStage0 stage0) >>= (`shouldBe` Right expectedStage1)

-- pos is dummy value
pos :: SourcePos
pos = (SourcePos "" pos1 pos1)