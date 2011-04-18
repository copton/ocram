module Ocram.Filter.Constraints (
	checkConstraints, getErrorCodes
) where

import Ocram.Filter.Util
import Ocram.Types
import Ocram.Visitor (UpVisitor(..), DownVisitor(..), traverseCTranslUnit)
import Language.C.Data.Ident (Ident(Ident))
import Language.C.Syntax.AST
import Data.Set (member)
import Data.Monoid (mconcat)

-- checkConstraints :: Context -> Result ValidAst {{{1
checkConstraints :: Context -> Result ValidAst
checkConstraints ctx = do
	ast <- getCyclefreeAst ctx
	cf <- getCriticalFunctions ctx
	fmap ValidAst $ performFilter (descriptor cf) $ getAst ast

getErrorCodes :: CriticalFunctions -> CyclefreeAst -> [Int]
getErrorCodes cf cyclefree_ast = performCheck (descriptor cf) $ getAst cyclefree_ast

printError :: Int -> String
printError 1 = "taking pointer from critical function"
printError x = error $ "unknown error code: " ++ show x

checker :: CriticalFunctions -> Ast -> [Error Int]
checker cf ast = checkFunctionPointer cf ast

descriptor cf = Filter "constraint" (checker cf) printError id

checkFunctionPointer :: CriticalFunctions -> Ast -> [Error Int]
checkFunctionPointer cf ast = snd $ traverseCTranslUnit ast cf

type DownState = CriticalFunctions
instance DownVisitor DownState

append ess code location = Error code location : pass ess
pass ess = mconcat ess

type UpState = [Error Int]
instance UpVisitor DownState UpState where
	upCExpr (CUnary CAdrOp (CVar (Ident name _ _ ) _ ) ni) cf es
		| name `member` cf = append es 1 ni 
		| otherwise = pass es
	upCExpr _ _ es = pass es
