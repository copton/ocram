module Ocram.Analysis.CriticalFunctions (
	determineCriticalFunctions
) where

import Ocram.Types (Result, CyclefreeAst, CriticalFunctions, FunctionMap, BlockingFunctions, CallGraph, Entry(Entry), Symbol)
import Ocram.Symbols (symbol)
import Language.C.Syntax.AST 
import Language.C.Data.Ident (Ident(Ident))
import qualified Data.Map as Map
import qualified Data.Set as Set

determineCriticalFunctions :: CyclefreeAst -> CallGraph -> FunctionMap -> BlockingFunctions -> Result CriticalFunctions
determineCriticalFunctions ciclefree_ast cg fm bfs = seq ciclefree_ast $ return $ 
	foldl (travBlocking (fm, cg)) Set.empty (Map.assocs bfs)

travEntry (fm, cg) cfs (Entry callers _) = foldl (travCaller (fm, cg)) cfs (Set.elems callers)

travCaller (fm, cg) cfs caller = 
	let fd = fm Map.! caller in
	let newCfs = Set.insert caller cfs in
	travEntry (fm, cg) newCfs $ cg Map.! caller

travBlocking (fm, cg) cfs (name, decl) = 
	let fid = symbol name in
	let newCfs = Set.insert fid cfs in
	case Map.lookup fid cg of
		Nothing -> newCfs
		(Just entry) -> travEntry (fm, cg) newCfs entry
