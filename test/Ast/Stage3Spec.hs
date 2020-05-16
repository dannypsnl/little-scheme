{-# LANGUAGE OverloadedStrings #-}
module Ast.Stage3Spec where
import Control.Monad.Except (runExceptT)
import SpecHelper
import Text.Megaparsec.Pos

import Scheme.Ast.Stage0
import Scheme.Ast.Stage1
import Scheme.Ast.Stage2
import Scheme.Ast.Stage3
import Scheme.Ast.Trans

spec :: Spec
spec = describe "transform" $ do
  context "stage2 -> stage3" $ do
    it "transform reuse part" $ do
      (Stage0_2 (Atom pos "aaa")) `transResultIs` (Stage0_3 (Atom pos "aaa"))
    it "let -> lambda" $ do
      (Let_2 pos [Binding_2 pos "a" (Stage0_2 (Number pos 1))]
                 [Stage0_2 (Atom pos "a")])
        `transResultIs`
        (Application_3 pos [Lambda_3 pos [Variable pos "a"] [Stage0_3 (Atom pos "a")], Stage0_3 (Number pos 1)])
  where
    transResultIs stage2 expectedStage3 = (runExceptT $ toStage3 stage2) >>= (`shouldBe` Right expectedStage3)
