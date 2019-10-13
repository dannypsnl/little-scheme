module SpecHelper (
  module Test.Hspec
  , str
) where

import Test.Hspec

import Language.Haskell.TH
import Language.Haskell.TH.Quote

str :: QuasiQuoter
str = QuasiQuoter { quoteExp = stringE }
