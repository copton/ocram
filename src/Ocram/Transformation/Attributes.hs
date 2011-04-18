module Ocram.Transformation.Attributes 
-- exports {{{1
(
	removeAttributes
) where

-- imports {{{1
import Ocram.Types
import Language.C.Syntax.AST
import Language.C.Data.Ident (Ident(Ident))
import qualified Data.Set as Set
import qualified Data.Map as Map
import Language.C.Pretty (pretty)
import Ocram.Names (blockingAttr, startRoutineAttr)

-- removeAttributes :: Context -> Result RevisedAst {{{1
removeAttributes :: Context -> Result RevisedAst
removeAttributes ctx = do
	ast <- getValidAst ctx
	bf <- getBlockingFunctions ctx
	sr <- getStartRoutines ctx
	return $ RevisedAst $ process bf sr $ getAst ast
	where

process bf sr (CTranslUnit decls ni) = CTranslUnit (map (revise bf sr) decls) ni

revise :: BlockingFunctions -> StartRoutines -> CExtDecl -> CExtDecl
revise bf sr cd@(CDeclExt (CDecl tts ds@[(Just (CDeclr (Just (Ident name _ _)) _ _ _ _), Nothing, Nothing)] ni))
	| Set.member name bf = CDeclExt (CDecl (filter (not . isAttr blockingAttr) tts) ds ni)
	| otherwise = cd

revise bf sr fd@(CFDefExt (CFunDef tts cd@(CDeclr (Just (Ident name _ _)) _ _ _ _) x y z))
	| Set.member name sr = CFDefExt (CFunDef (filter (not . isAttr startRoutineAttr) tts) cd x y z)
	| otherwise = fd

isAttr name (CTypeQual (CAttrQual (CAttr (Ident name' _ _) _ _))) = name == name'
isAttr _ _ = False
