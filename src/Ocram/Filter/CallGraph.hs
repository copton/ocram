module Ocram.Filter.CallGraph 
-- exports {{{1
(
	check_call_graph --, getErrorCodes
) where

-- imports {{{1
import Ocram.Filter.Util
import Ocram.Query (getFunDefs)
import Ocram.Types
import Language.C.Data.Node (nodeInfo)
import Language.C.Syntax.AST (CFunDef)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (reverse, intersperse, partition)
import Data.Set (toList)

-- checkCallGraph :: Context -> Result ForestAst  {{{1
check_call_graph ::  CallGraph -> StartRoutines -> DefinedFunctions -> Ast -> ER ()
check_call_graph cg sr df ast = 
	let df' = getFunDefs ast df in
	performFilter (descriptor cg sr df') ast

-- getErrorCodes :: Context -> Result [Int] {{{1
--getErrorCodes :: Context -> Result [Int]
--getErrorCodes ctx = do
--	ast <- getSaneAst ctx
--	cg <- getCallGraph ctx
--	sr <- getStartRoutines ctx
--	df <- getDefinedFunctions ctx
--	let df' = getFunDefs (getAst ast) df
--	return $ performCheck (descriptor cg sr df') $ getAst ast

-- utils {{{1

data MyError = 
	  RecError [Symbol]
	| ConError Symbol

descriptor cg sr df' = 
	Filter "recursion" (checker cg sr df') (printError) errorId

checker :: CallGraph -> StartRoutines -> FunMap CFunDef -> Ast -> [Error MyError]
checker cg sr fm _ = conErrors ++ recErrors
	where
		(conn, disc) = partition (flip Map.member cg) $ Set.elems sr
		conErrors = map (createError . ConError) disc
		recErrors = map createError $ concatMap (check cg []) conn
		createError re@(RecError (_:function:_)) = Error re $ nodeInfo $ fm Map.! function
		createError ce@(ConError function) = Error ce $ nodeInfo $ fm Map.! function

check :: CallGraph -> [Symbol] -> Symbol -> [MyError]
check call_graph call_stack function = 
	let call_stack' = function : call_stack in
	let callees = toList $ cgCallees $ call_graph Map.! function in
	if function `elem` call_stack then
		[RecError call_stack']
	else
		concatMap (check call_graph call_stack') callees
		
printError :: MyError -> String
printError (RecError symbols) =
	"recursive call of critical function '" ++ (head symbols) ++ "'.\nCall stack is '" ++ call_stack symbols ++ "'"
printError (ConError symbol) =
	"start routine '" ++ symbol ++ "' does not call any critical or blocking functions."

call_stack symbols = concat $ intersperse "' -> '" $ reverse symbols

errorId :: MyError -> Int
errorId (RecError _) = 1
errorId (ConError _) = 2
