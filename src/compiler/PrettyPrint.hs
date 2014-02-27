module PrettyPrint where
import Parser
prettyPrint :: Expr -> String
prettyPrint ast = helper ast 0

helper :: Expr -> Int -> String
helper (Sum e1 e2) ilevel =
  (indent ilevel) ++ "SUM " ++ (helper e1 (ilevel+1))
  ++ " " ++ helper e2 (ilevel + 1)
helper (Num a) _ = (show a)

indent l = replicate l ' '
  
