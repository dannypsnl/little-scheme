{-# LANGUAGE OverloadedStrings #-}
module Ast.Stage2Spec where
import Control.Monad.Except (runExceptT)
import SpecHelper
import Text.Megaparsec.Pos

import Scheme.Ast.Stage0
import Scheme.Ast.Stage1
import Scheme.Ast.Stage2
import Scheme.Ast.Trans

spec :: Spec
spec = describe "transform" $ do
  context "stage1 -> stage2" $ do
    it "transform reuse part" $ do
      (Stage0 (Atom pos "aaa")) `transResultIs` (Stage0_2 (Atom pos "aaa"))
    it "let* form" $ do
      (LetStar pos [Binding pos "a" (Stage0 (Number pos 1))
                    , Binding pos "b" (Stage0 (Atom pos "a"))]
                   [Stage0 (Atom pos "b")])
        `transResultIs`
        (Let_2 pos [Binding_2 pos "a" (Stage0_2 (Number pos 1))]
                   [(Let_2 pos [Binding_2 pos "b" (Stage0_2 (Atom pos "a"))]
                               [Stage0_2 (Atom pos "b")])])
    it "letrec form" $ do
      (LetRec pos [Binding pos "a" (Stage0 (Number pos 1))
                   , Binding pos "b" (Stage0 (Number pos 1))]
                  [Stage0 (Atom pos "a")])
        `transResultIs`
        (Let_2 pos [Binding_2 pos "a" (Stage0_2 $ Quoted pos (Atom pos "a"))
                    , Binding_2 pos "b" (Stage0_2 $ Quoted pos (Atom pos "b"))]
                   [Set_2 pos "a" (Stage0_2 (Number pos 1))
                    , Set_2 pos "b" (Stage0_2 (Number pos 1))
                    , Stage0_2 (Atom pos "a")])
  where
    transResultIs stage1 expectedStage2 = (runExceptT $ toStage2 stage1) >>= (`shouldBe` Right expectedStage2)

