module Ocram.Filter.Constraints (
	checkConstraints, getErrorCodes
) where

import Ocram.Filter.Util (Error, pass, append, Filter(Filter), performCheck, performFilter)
import Ocram.Types (Result, Ast, getAst, CyclefreeAst, ValidAst(ValidAst))
import Ocram.Analysis.Types (CriticalFunctions)
import Ocram.Visitor (UpVisitor(..), DownVisitor(..), traverseCTranslUnit)
import Language.C.Data.Ident (Ident(Ident))
import Language.C.Syntax.AST
import Data.Map (member)

checkConstraints :: CriticalFunctions -> CyclefreeAst -> Result ValidAst
checkConstraints cf cyclefree_ast = fmap ValidAst $ performFilter (descriptor cf) $ getAst cyclefree_ast

getErrorCodes :: CriticalFunctions -> CyclefreeAst -> [Int]
getErrorCodes cf cyclefree_ast = performCheck (descriptor cf) $ getAst cyclefree_ast

errorCodes :: Int -> String
errorCodes 1 = "taking pointer from critical function"
errorCodes x = error $ "unknown error code: " ++ show x

checker :: CriticalFunctions -> Ast -> [Error]
checker cf ast = checkFunctionPointer cf ast

descriptor cf = Filter "constraint" (checker cf) errorCodes

checkFunctionPointer :: CriticalFunctions -> Ast -> [Error]
checkFunctionPointer cf ast = snd $ traverseCTranslUnit ast cf

type DownState = CriticalFunctions
instance DownVisitor DownState

type UpState = [Error]
instance UpVisitor DownState UpState where
	upCExpr (CUnary CAdrOp (CVar (Ident name _ _ ) _ ) ni) cf es
		| name `member` cf = append es 1 ni 
		| otherwise = pass es
	upCExpr _ _ es = pass es
