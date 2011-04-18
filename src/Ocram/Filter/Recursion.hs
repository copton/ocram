module Ocram.Filter.Recursion 
-- exports {{{1
(
	checkRecursion, getErrorCodes
) where

-- imports {{{1
import Ocram.Filter.Util
import Ocram.Query (getFunDefs)
import Ocram.Types
import Language.C.Data.Node (nodeInfo)
import Language.C.Syntax.AST (CFunDef)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (reverse, intersperse)
import Data.Set (toList)

-- checkRecursion :: Context -> Result CyclefreeAst  {{{1
checkRecursion :: Context -> Result CyclefreeAst 
checkRecursion ctx = do
	ast <- getSaneAst ctx
	cg <- getCallGraph ctx
	sr <- getStartRoutines ctx
	df <- getDefinedFunctions ctx
	let df' = getFunDefs ast df
	fmap CyclefreeAst $ performFilter (descriptor cg sr df') $ getAst ast

-- getErrorCodes :: Context -> Result [Int] {{{1
getErrorCodes :: Context -> Result [Int]
getErrorCodes ctx = do
	ast <- getSaneAst ctx
	cg <- getCallGraph ctx
	sr <- getStartRoutines ctx
	df <- getDefinedFunctions ctx
	let df' = getFunDefs ast df
	return $ performCheck (descriptor cg sr df') $ getAst ast

-- utils {{{1

newtype RecError = RecError [Symbol]

descriptor cg sr df' = 
	Filter "recursion" (checker cg sr df') (printError) (const 1)

checker :: CallGraph -> StartRoutines -> FunMap CFunDef -> Ast -> [Error RecError]
checker cg srs fm _ = map createError $ concatMap (check cg []) $ Set.elems srs
	where
		createError re@(RecError (_:function:_)) = Error re $ nodeInfo $ fm Map.! function

check :: CallGraph -> [Symbol] -> Symbol -> [RecError]
check call_graph call_stack function = 
	let call_stack' = function : call_stack in
	let callees = toList $ cgCallees $ call_graph Map.! function in
	if function `elem` call_stack then
		[RecError call_stack']
	else
		concatMap (check call_graph call_stack') callees
		
printError :: RecError -> String
printError (RecError symbols) =
	"recursive call of critical function '" ++ (head symbols) ++ "'.\nCall stack is '" ++ call_stack ++ "'"
	where
		call_stack = concat $ intersperse "' -> '" $ reverse symbols
