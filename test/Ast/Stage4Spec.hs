{-# LANGUAGE OverloadedStrings #-}
module Ast.Stage4Spec where
import Control.Monad.Except (runExceptT)
import Data.Either
import SpecHelper
import Text.Megaparsec.Pos

import Scheme.Ast.Stage0
import Scheme.Ast.Stage1
import Scheme.Ast.Stage3
import Scheme.Ast.Stage4
import Scheme.Ast.Trans

spec :: Spec
spec = describe "transform" $ do
  context "stage3 -> stage4" $ do
    it "transform reuse part" $ do
      (Stage0_3 (Atom pos "aaa")) `transResultIs` (Stage0_4 (Atom pos "aaa"))
    it "define variable form" $ do
      (Define_3 pos "x" Nothing [Stage0_3 (Number pos 1)])
        `transResultIs`
        (Define_4 pos "x" (Stage0_4 (Number pos 1)))
    it "reject bad define variable form" $ do
      (runExceptT $ toStage4 (Define_3 pos "x" Nothing [Stage0_3 (Number pos 1), Stage0_3 (Number pos 2)]))
        >>= (\result -> isLeft result `shouldBe` True)
    it "simplify define function form to define variable form" $ do
      (Define_3 pos "id" (Just [Variable pos "a"])
                         [Stage0_3 (Atom pos "a")])
        `transResultIs`
        (Define_4 pos "id"
          (Lambda_4 pos [Variable pos "a"] [Stage0_4 (Atom pos "a")]))
  where
    transResultIs stage3 expectedStage4 = (runExceptT $ toStage4 stage3) >>= (`shouldBe` Right expectedStage4)

-- pos is dummy value
pos :: SourcePos
pos = (SourcePos "" pos1 pos1)
