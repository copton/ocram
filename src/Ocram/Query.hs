module Ocram.Query 
--- exports {{{1
(
	getFunDefs, getFunDecls
) where

-- imports {{{1
import Ocram.Types (AstC, getAst, Symbol)
import Ocram.Symbols (symbol)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Language.C.Syntax.AST(CFunDef, CDecl, CExternalDeclaration(CFDefExt, CDeclExt))
import Ocram.Visitor (traverseCTranslUnit, DownVisitor, UpVisitor(upCExtDecl))

-- getFunDefs :: (AstC a) => a -> Set.Set Symbol -> Map.Map Symbol CFunDef {{{1
getFunDefs :: (AstC a) => a -> Set.Set Symbol -> Map.Map Symbol CFunDef
getFunDefs ast symbols = foldl createEntry Map.empty $ snd $ traverseCTranslUnit (getAst ast) symbols

type DownState = Set.Set Symbol
instance DownVisitor DownState

type UpState = [CFunDef]
instance UpVisitor DownState UpState where
	upCExtDecl (CFDefExt fd) symbols _
		| Set.member (symbol fd) symbols = [fd]
		| otherwise = []

-- getFunDecls :: (AstC a) => a -> Set.Set Symbol -> Map.Map Symbol CDecl {{{1
getFunDecls :: (AstC a) => a -> Set.Set Symbol -> Map.Map Symbol CDecl
getFunDecls ast symbols = foldl createEntry Map.empty $ snd $ traverseCTranslUnit (getAst ast) symbols

type UpState' = [CDecl]
instance UpVisitor DownState UpState' where
	upCExtDecl (CDeclExt fd) symbols _
		| Set.member (symbol fd) symbols = [fd]
		| otherwise = []

-- util {{{1
createEntry m fd = Map.insert (symbol fd) fd m
