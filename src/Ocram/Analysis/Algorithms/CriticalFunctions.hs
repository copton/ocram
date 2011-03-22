module Ocram.Analysis.Algorithms.CriticalFunctions (
	determineCriticalFunctions
) where

import Ocram.Analysis.Types.FunctionMap (functionId)
import Ocram.Analysis.Types.CriticalFunctions (CriticalFunctions)
import Ocram.Analysis.Types.CallGraph (CallGraph, Entry(Entry))
import Ocram.Context(Context(Context, ctxBlockingFunctions, ctxCallGraph))
import qualified Data.Set as Set
import qualified Data.Map as Map

travEntry cg cfs (Entry callers _) = foldl (travCaller cg) cfs (Set.elems callers)

travCaller cg cfs caller = 
	let newCfs = Set.insert caller cfs in
	travEntry cg newCfs $ cg Map.! caller

travBlocking cg cfs name = 
	let fid = functionId name in
	let newCfs = Set.insert fid cfs in
	case Map.lookup fid cg of
		Nothing -> newCfs
		(Just entry) -> travEntry cg newCfs entry

determineCriticalFunctions :: Context -> CriticalFunctions
determineCriticalFunctions ctx = 
	let bfs = ctxBlockingFunctions ctx in
	let cg = ctxCallGraph ctx in
	foldl (travBlocking cg) Set.empty (Map.keys bfs)
