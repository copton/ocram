module Ocram.Query 
--- exports {{{1
(
	getFunDefs, getFunDecls, getCallChain
) where

-- imports {{{1
import Ocram.Types
import Ocram.Symbols (symbol)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Language.C.Syntax.AST(CFunDef, CDecl, CExternalDeclaration(CFDefExt, CDeclExt))
import Ocram.Visitor (traverseCTranslUnit, DownVisitor, UpVisitor(upCExtDecl))


-- getCallChain :: CallGraph -> Symbol -> [Symbol] {{{1
getCallChain :: CallGraph -> Symbol -> [Symbol]
getCallChain cg fName = topologicSort cg Set.empty fName

topologicSort cg names fName =
	if Set.member fName names
		then []
		else 
			let
				names' = Set.insert fName names
				callees = cgCallees $ cg Map.! fName
				rest = concatMap (topologicSort cg names') $ Set.elems callees
			in
				fName : rest
			
-- getFunDefs :: Ast -> Set.Set Symbol -> Map.Map Symbol CFunDef {{{1
getFunDefs :: Ast -> Set.Set Symbol -> Map.Map Symbol CFunDef
getFunDefs ast symbols = foldl createEntry Map.empty $ snd $ traverseCTranslUnit ast symbols

type DownState = Set.Set Symbol
instance DownVisitor DownState

type UpState = [CFunDef]
instance UpVisitor DownState UpState where
	upCExtDecl (CFDefExt fd) symbols _
		| Set.member (symbol fd) symbols = [fd]
		| otherwise = []
	upCExtDecl _ _ _ = []

-- getFunDecls :: Ast -> Set.Set Symbol -> Map.Map Symbol CDecl {{{1
getFunDecls :: Ast -> Set.Set Symbol -> Map.Map Symbol CDecl
getFunDecls ast symbols = foldl createEntry Map.empty $ snd $ traverseCTranslUnit ast symbols

type UpState' = [CDecl]
instance UpVisitor DownState UpState' where
	upCExtDecl (CDeclExt fd) symbols _
		| Set.member (symbol fd) symbols = [fd]
		| otherwise = []
	upCExtDecl _ _ _ = []

-- util {{{1
createEntry m fd = Map.insert (symbol fd) fd m
