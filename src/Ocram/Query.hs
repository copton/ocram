module Ocram.Query 
-- exports {{{1
(
	getFunDefs, getFunDecls, getCallChain
) where

-- imports {{{1
import Ocram.Types
import Ocram.Symbols (symbol)
import Ocram.Visitor (traverseCTranslUnit, DownVisitor, UpVisitor(upCExtDecl), ListVisitor)

import Language.C.Syntax.AST(CFunDef, CDecl, CExternalDeclaration(CFDefExt, CDeclExt))

import qualified Data.Set as Set
import qualified Data.Map as Map

import Control.Exception (assert)

-- getCallChain :: CallGraph -> Symbol -> [Symbol] {{{1
getCallChain :: CallGraph -> Symbol -> [Symbol]
getCallChain cg fName = assert (Map.member fName cg) $ topologicSort cg Set.empty fName

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
getFunDefs ast symbols = foldl createEntry Map.empty $ snd $ traverseCTranslUnit ast (DownState symbols)

newtype DownState = DownState (Set.Set Symbol)
instance DownVisitor DownState

type UpState = [CFunDef]
instance UpVisitor DownState UpState where
	upCExtDecl o@(CFDefExt fd) (DownState symbols) _
		| Set.member (symbol fd) symbols = (o, [fd])
		| otherwise = (o, [])
	upCExtDecl o _ _ = (o, [])

instance ListVisitor DownState UpState

-- getFunDecls :: Ast -> Set.Set Symbol -> Map.Map Symbol CDecl {{{1
getFunDecls :: Ast -> Set.Set Symbol -> Map.Map Symbol CDecl
getFunDecls ast symbols = foldl createEntry Map.empty $ snd $ traverseCTranslUnit ast (DownState symbols)

type UpState' = [CDecl]
instance UpVisitor DownState UpState' where
	upCExtDecl o@(CDeclExt fd) (DownState symbols) _
		| Set.member (symbol fd) symbols = (o, [fd])
		| otherwise = (o, [])
	upCExtDecl o _ _ = (o, [])

instance ListVisitor DownState UpState'

-- util {{{1
createEntry m fd = Map.insert (symbol fd) fd m
