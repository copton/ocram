module Ocram.Filter.Constraints (
	checkConstraints, getErrorCodes
) where

import Ocram.Filter.Util
import Ocram.Types
import Ocram.Visitor (UpVisitor(..), DownVisitor(..), traverseCTranslUnit)
import Language.C.Data.Ident (Ident(Ident))
import Language.C.Syntax.AST
import qualified Data.Set as Set
import Data.Monoid (mconcat)

-- checkConstraints :: Context -> Result ValidAst {{{1
checkConstraints :: Context -> Result ValidAst
checkConstraints ctx = do
	ast <- getForestAst ctx
	cf <- getCriticalFunctions ctx
	sr <- getStartRoutines ctx
	fmap ValidAst $ performFilter (descriptor cf sr) $ getAst ast

-- getErrorCodes :: Context -> Result [Int] {{{1
getErrorCodes :: Context -> Result [Int]
getErrorCodes ctx = do
	ast <- getForestAst ctx	
	cf <- getCriticalFunctions ctx
	sr <- getStartRoutines ctx
	return $ performCheck (descriptor cf sr) $ getAst ast

-- utils {{{1
printError :: Int -> String
printError 1 = "taking pointer from critical function"
printError 2 = "at least one thread must be started"
printError x = error $ "unknown error code: " ++ show x

checker :: CriticalFunctions -> StartRoutines -> Ast -> [Error Int]
checker cf sr ast = checkFunctionPointer cf ast ++ checkThreads ast sr

descriptor cf sr = Filter "constraint" (checker cf sr) printError id

checkThreads (CTranslUnit _ ni) sr 
	| Set.null sr = [Error 2 ni]
	| otherwise = []

checkFunctionPointer :: CriticalFunctions -> Ast -> [Error Int]
checkFunctionPointer cf ast = snd $ traverseCTranslUnit ast $ DownState cf

newtype DownState = DownState CriticalFunctions
instance DownVisitor DownState

append es code location = Error code location : es

type UpState = [Error Int]
instance UpVisitor DownState UpState where
	upCExpr (CUnary CAdrOp (CVar (Ident name _ _ ) _ ) ni) (DownState cf) u
		| name `Set.member` cf = append u 1 ni 
		| otherwise = u
	upCExpr _ _ u = u
