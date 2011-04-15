module Ocram.Transformation.Attributes (
	removeAttributes
) where

import Ocram.Types (ValidAst, BlockingFunctions, StartRoutines, RevisedAst(RevisedAst), getAst, Result)
import Language.C.Syntax.AST
import Language.C.Data.Ident (Ident(Ident))
import qualified Data.Set as Set
import qualified Data.Map as Map

import Language.C.Pretty (pretty)
import Debug.Trace (trace)

removeAttributes :: ValidAst -> BlockingFunctions -> StartRoutines -> Result RevisedAst
removeAttributes valid_ast bf sr = return $ RevisedAst $ process $ getAst valid_ast
	where
		process (CTranslUnit decls ni) = CTranslUnit (map (revise bf sr) decls) ni

revise :: BlockingFunctions -> StartRoutines -> CExtDecl -> CExtDecl
revise bf sr cd@(CDeclExt (CDecl tts ds@[(Just (CDeclr (Just (Ident name _ _)) _ _ _ _), Nothing, Nothing)] ni))
	| Map.member name bf = CDeclExt (CDecl (filter (not . isAttr "tc_blocking") tts) ds ni)
	| otherwise = cd

revise bf sr fd@(CFDefExt (CFunDef tts cd@(CDeclr (Just (Ident name _ _)) _ _ _ _) x y z))
	| elem name sr = CFDefExt (CFunDef (filter (not . isAttr "tc_run_thread") tts) cd x y z)
	| otherwise = fd

isAttr name (CTypeQual (CAttrQual (CAttr (Ident name' _ _) _ _))) = name == name'
isAttr _ _ = False
