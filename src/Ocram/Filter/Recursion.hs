module Ocram.Filter.Recursion (
	checkRecursion, getErrorCodes
) where

import Ocram.Filter.Util (Error(Error), Filter(Filter), performCheck, performFilter)
import Ocram.Types (Result, Ast, getAst, SaneAst, CyclefreeAst(CyclefreeAst))
import Ocram.Symbols (Symbol)
import Ocram.Analysis (CallGraph, StartRoutines, FunctionMap, cgCallees)
import Language.C.Data.Node (nodeInfo)
import qualified Data.Map as Map
import Data.List (reverse, intersperse)
import Data.Set (toList)

newtype RecError = RecError [Symbol]

checkRecursion :: SaneAst -> CallGraph -> StartRoutines -> FunctionMap -> Result CyclefreeAst 
checkRecursion sane_ast call_graph start_routines function_map = 
	fmap CyclefreeAst $ performFilter (descriptor call_graph start_routines function_map) $ getAst sane_ast

getErrorCodes :: SaneAst -> CallGraph -> StartRoutines -> FunctionMap -> [Int]
getErrorCodes sane_ast call_graph start_routines function_map = 
	performCheck (descriptor call_graph start_routines function_map) $ getAst sane_ast

descriptor call_graph start_routines function_map = 
	Filter "recursion" (checker call_graph start_routines function_map) (printError) (const 1)

checker :: CallGraph -> StartRoutines -> FunctionMap -> Ast -> [Error RecError]
checker cg srs fm _ = map createError $ concatMap (check cg []) srs
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
