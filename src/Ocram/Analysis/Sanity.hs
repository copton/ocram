module Ocram.Analysis.Sanity 
-- exports {{{1
(
	check_sanity
) where

-- imports {{{1
import Ocram.Analysis.Filter
import Ocram.Types
import Ocram.Visitor (UpVisitor(..), EmptyDownState, emptyDownState, traverseCTranslUnit, ListVisitor)
import Language.C.Syntax.AST 
import Data.Monoid (mconcat)

-- check_sanity :: Ast -> ER () {{{1
check_sanity :: Ast -> ER ()
check_sanity ast = performFilter descriptor ast


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
