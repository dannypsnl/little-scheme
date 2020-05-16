module SpecHelper (
  module Test.Hspec
  , str
  , pos
) where

import Test.Hspec

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Text.Megaparsec.Pos

str :: QuasiQuoter
str = QuasiQuoter { quoteExp = stringE }

-- pos is dummy value
pos :: SourcePos
pos = (SourcePos "" pos1 pos1)
