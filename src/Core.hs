module Core (
  ScmValue(..),
  unwordsList
) where

data ScmValue =
  Atom String
  | List [ScmValue]
  | Pair [ScmValue] ScmValue
  | Number Integer
  | String String
  | Bool Bool

instance Show ScmValue where
  show = showValue

showValue :: ScmValue -> String
showValue (String str) = "\"" ++ str ++ "\""
showValue (Atom name) = name
showValue (Number num) = show num
showValue (Bool True) = "#t"
showValue (Bool False) = "#f"
showValue (List contents) = "(" ++ unwordsList contents ++ ")"
showValue (Pair head tail) = "(" ++ unwordsList head ++ " . " ++ showValue tail ++ ")"

unwordsList :: [ScmValue] -> String
unwordsList = unwords . map showValue
