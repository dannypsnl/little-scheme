module Scheme.Literal (
  litFile
) where
import Language.Haskell.TH
import Language.Haskell.TH.Quote

litFile :: QuasiQuoter
litFile = quoteFile lit

literally :: String -> Q Exp
literally = return . LitE . StringL

lit :: QuasiQuoter
lit = QuasiQuoter { quoteExp = literally }
