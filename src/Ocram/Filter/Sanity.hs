module Ocram.Filter.Sanity 
-- exports {{{1
(
	checkSanity, getErrorCodes
) where

-- imports {{{1
import Ocram.Filter.Util (Error(Error), Filter(Filter), performFilter, performCheck)
import Ocram.Types
import Ocram.Visitor (UpVisitor(..), EmptyDownState, emptyDownState, traverseCTranslUnit, ListVisitor)
import Language.C.Syntax.AST 
import Data.Monoid (mconcat)

-- checkSanity :: Context -> Result SaneAst {{{1
checkSanity :: Context -> Result SaneAst
checkSanity ctx = do
	let ast = getRawAst ctx
	fmap SaneAst $ performFilter descriptor $ getAst ast

-- getErrorCodes :: Context -> Result [Int] {{{1
getErrorCodes :: Context -> Result [Int]
getErrorCodes ctx = do
	let ast = getRawAst ctx
	return $ performCheck descriptor $ getAst ast

-- util {{{1
checker :: Ast -> [Error Int]
checker ast = snd $ traverseCTranslUnit ast emptyDownState

printError :: Int -> String
printError 1 = "function without parameter list"
printError x = error $ "unknown error code " ++ show x

descriptor = Filter "sanity" checker printError id

append es code location = Error code location : es

type UpState = [Error Int]

instance UpVisitor EmptyDownState UpState where
	upCExtDecl o@(CFDefExt (CFunDef _ (CDeclr _ [] _ _ _) _ _ ni)) _ u = (o, append u 1 ni)
	upCExtDecl o _ u = (o, u)

instance ListVisitor EmptyDownState UpState
