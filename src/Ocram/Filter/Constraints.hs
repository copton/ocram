module Ocram.Filter.Constraints (
	checkConstraints, getErrorCodes
) where

import Ocram.Filter.Util (Error, pass, append, Filter(Filter), performCheck, performFilter)
import Ocram.Types (AST)
import Ocram.Analysis.Types (CriticalFunctions)
import Ocram.Visitor (UpVisitor(..), DownVisitor(..), traverseCTranslUnit)
import Language.C.Data.Ident (Ident(Ident))
import Language.C.Syntax.AST
import Data.Map (member)

errorCodes :: Int -> String
errorCodes 1 = "taking pointer from critical function"
errorCodes x = error $ "unknown error code: " ++ show x

checker :: CriticalFunctions -> AST -> [Error]
checker cf ast = checkFunctionPointer cf ast

descriptor cf = Filter "constraint" (checker cf) errorCodes

checkConstraints cf = performFilter (descriptor cf)
getErrorCodes cf = performCheck (descriptor cf)

checkFunctionPointer :: CriticalFunctions -> AST -> [Error]
checkFunctionPointer cf ast = snd $ traverseCTranslUnit ast cf

type DownState = CriticalFunctions
instance DownVisitor DownState

type UpState = [Error]
instance UpVisitor DownState UpState where
	upCExpr (CUnary CAdrOp (CVar (Ident name _ _ ) _ ) ni) cf es
		| name `member` cf = append es 1 ni 
		| otherwise = pass es
	upCExpr _ _ es = pass es
