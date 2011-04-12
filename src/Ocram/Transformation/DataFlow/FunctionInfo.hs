module Ocram.Transformation.DataFlow.FunctionInfo (
	retrieveFunctionInfos
) where 

import Ocram.Analysis (FunctionMap, CriticalFunctions)
import Ocram.Transformation.Types (FunctionInfos, FunctionInfo(FunctionInfo), Frame)
import Data.Monoid (Monoid, mappend, mempty, mconcat)
import Ocram.Visitor (traverseCFunDef, emptyDownState, EmptyDownState, UpVisitor(mapCStat))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (partition)
import Data.Maybe (fromJust)
import Language.C.Syntax.AST

retrieveFunctionInfos :: FunctionMap -> CriticalFunctions -> FunctionInfos
retrieveFunctionInfos fm cf = Map.map retrieveFunctionInfo $ Map.filterWithKey (\k _ -> Set.member k cf) fm

retrieveFunctionInfo :: CFunDef -> FunctionInfo
retrieveFunctionInfo fd@(CFunDef tss (CDeclr _ [cfd] Nothing _ _) [] _ _) = 
	FunctionInfo resultType (params ++ variables) body
	where
		(variables, body) = process fd
		params = extractParams cfd
		resultType = extractTypeSpec tss

process :: CFunDef -> (Frame, [CBlockItem])
process fd = (frame, body)
	where
		(maybeFd, frame) = traverseCFunDef fd emptyDownState
		body = extractBody $ fromJust maybeFd

type UpState = Frame

instance UpVisitor EmptyDownState UpState where
	mapCStat (CCompound idents items ni) _ us = (Just (CCompound idents items' ni), upState)
		where
			(decls, items') = partition isCBlockDecl items
			upState = map (\(CBlockDecl d) -> d) decls ++ mconcat us
	mapCStat _ _ us = (Nothing, mconcat us)
		 
isCBlockDecl (CBlockDecl _) = True
isCBlockDecl _ = True

extractBody :: CFunDef -> [CBlockItem]
extractBody (CFunDef _ _ _ (CCompound _ items _) _) = items

extractParams :: CDerivedDeclr -> [CDecl]
extractParams (CFunDeclr (Right (ps, False)) _ _) = ps

extractTypeSpec :: [CDeclSpec] -> CTypeSpec
extractTypeSpec [] = error "ill-formed input: type specifier expected"
extractTypeSpec ((CTypeSpec ts):xs) = ts
extractTypeSpec (_:xs) = extractTypeSpec xs
