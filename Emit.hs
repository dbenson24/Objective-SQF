module Emit where

import qualified Syntax as S
import Numeric

codegen :: S.Expr -> String

--codegen expr = 
--    "test"

codegen (S.Float f) =
    showFFloat Nothing f " "

codegen (S.BinOp op ls rs) =
    (codegen ls) ++ " " ++ show op ++ " " ++ (codegen rs)

codegen (S.Var str) = 
    " " ++ str ++ " "

codegen (S.Call name args) =
    (concat $ map codegen args) ++ " call " ++ name

codegen (S.Function name args body) =
    name ++ " = " ++ "{ " ++ (concat $ map codegen args) ++ (concat handleBody) ++ " }"
    
    where handleBody = case body of S.Block block -> map codegen block
                                    expr -> ["{ ", codegen expr, " };"]
codegen (S.Block exprs) =
    "{ \n" ++ (concat $ map codegen exprs) ++ " };\n"

codegen (S.If cond tr fl) =
    "if (" ++ codegen cond ++ ") then \n{ " ++ codegen tr  ++ " \n} else { " ++ codegen fl ++ "};\n"

codegenTop :: [S.Expr] -> String
codegenTop exprs =
    concat $ map codegen exprs

-- handleArguments args  
