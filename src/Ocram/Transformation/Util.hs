module Ocram.Transformation.Util 
-- exports {{{1
(
	ident, un
	, removeAttributes
) where

-- imports {{{1
import Ocram.Types
import qualified Data.Set as Set
import Language.C.Data.Ident (Ident(Ident))
import Language.C.Data.Node (undefNode)
import Language.C.Syntax.AST
import Ocram.Names (blockingAttr, startRoutineAttr)

-- ident {{{1
ident s = Ident s 0 un
un = undefNode


-- removeAttributes :: BlockingFunctions -> StartRoutines -> Ast -> Ast {{{1
removeAttributes :: BlockingFunctions -> StartRoutines -> Ast -> Ast
removeAttributes bf sr (CTranslUnit decls ni) = CTranslUnit (map (revise bf sr) decls) ni

revise :: BlockingFunctions -> StartRoutines -> CExtDecl -> CExtDecl
revise bf sr cd@(CDeclExt (CDecl tts ds@[(Just (CDeclr (Just (Ident name _ _)) _ _ _ _), Nothing, Nothing)] ni))
	| Set.member name bf = CDeclExt (CDecl (filter (not . isAttr blockingAttr) tts) ds ni)
	| otherwise = cd

revise bf sr fd@(CFDefExt (CFunDef tts cd@(CDeclr (Just (Ident name _ _)) _ _ _ _) x y z))
	| Set.member name sr = CFDefExt (CFunDef (filter (not . isAttr startRoutineAttr) tts) cd x y z)
	| otherwise = fd

isAttr name (CTypeQual (CAttrQual (CAttr (Ident name' _ _) _ _))) = name == name'
isAttr _ _ = False
