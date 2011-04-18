module Ocram.Analysis.CriticalFunctions 
-- exports {{{1
(
	determineCriticalFunctions
) where

-- imports {{{1
import Ocram.Types
import Ocram.Symbols (symbol)
import Language.C.Syntax.AST 
import Language.C.Data.Ident (Ident(Ident))
import qualified Data.Map as Map
import qualified Data.Set as Set

-- determineCriticalFunctions :: Context -> Result CriticalFunctions {{{1
determineCriticalFunctions :: Context -> Result CriticalFunctions
determineCriticalFunctions ctx = do
	ast <- getCyclefreeAst ctx
	cg <- getCallGraph ctx
	bf <- getBlockingFunctions ctx
	seq ast $ return $ foldl (travBlocking cg) Set.empty (Set.elems bf)

travEntry cg cfs (Entry callers _) = foldl (travCaller cg) cfs (Set.elems callers)

travCaller cg cfs caller = 
	let newCfs = Set.insert caller cfs in
	travEntry cg newCfs $ cg Map.! caller

travBlocking cg cfs name = 
	let fid = symbol name in
	let newCfs = Set.insert fid cfs in
	case Map.lookup fid cg of
		Nothing -> newCfs
		(Just entry) -> travEntry cg newCfs entry
