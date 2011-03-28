module Ocram.Analysis.CriticalFunctions (
	determineCriticalFunctions
) where

import Ocram.Analysis.Types (CriticalFunctions, FunctionMap, BlockingFunctions, Signature(Signature), CallGraph, Entry(Entry))
import Ocram.Types (Result)
import Ocram.Symbols (symbol, Symbol)
import Language.C.Syntax.AST 
import Language.C.Data.Ident (Ident(Ident))
import qualified Data.Map as Map
import qualified Data.Set as Set

determineCriticalFunctions :: CallGraph -> FunctionMap -> BlockingFunctions -> Result CriticalFunctions
determineCriticalFunctions cg fm bfs = return $ 
	foldl (travBlocking (fm, cg)) Map.empty (Map.assocs bfs)

travEntry (fm, cg) cfs (Entry callers _) = foldl (travCaller (fm, cg)) cfs (Set.elems callers)

travCaller (fm, cg) cfs caller = 
	let fd = fm Map.! caller in
	let newCfs = Map.insert caller (def2sig fd) cfs in
	travEntry (fm, cg) newCfs $ cg Map.! caller

travBlocking (fm, cg) cfs (name, decl) = 
	let fid = symbol name in
	let newCfs = Map.insert fid (decl2sig decl) cfs in
	case Map.lookup fid cg of
		Nothing -> newCfs
		(Just entry) -> travEntry (fm, cg) newCfs entry


def2sig :: CFunDef -> Signature
def2sig (CFunDef tss (CDeclr _ [cfd] Nothing _ _) [] _ _) = Signature (extractTypeSpec tss) (extractParams cfd)

decl2sig :: CExtDecl -> Signature
decl2sig (CDeclExt (CDecl tss [(Just (CDeclr _ [cfd] Nothing _ _), Nothing, Nothing)]_)) = Signature (extractTypeSpec tss) (extractParams cfd)

extractParams :: CDerivedDeclr -> [(CTypeSpec, Symbol)]
extractParams (CFunDeclr (Right (ps, False)) _ _) = map extractParam ps

extractParam :: CDecl -> (CTypeSpec, Symbol)
extractParam (CDecl tss [(Just (CDeclr (Just (Ident name _ _)) [] Nothing _ _), Nothing, Nothing)] _) = (extractTypeSpec tss, name)

extractTypeSpec :: [CDeclSpec] -> CTypeSpec
extractTypeSpec [] = error "ill-formed input: type specifier expected"
extractTypeSpec ((CTypeSpec ts):xs) = ts
extractTypeSpec (_:xs) = extractTypeSpec xs
