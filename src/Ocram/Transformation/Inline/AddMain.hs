module Ocram.Transformation.Inline.AddMain(
  addMain
) where

import qualified Data.Set as Set
import Language.C.Syntax.AST
import Language.C.Syntax.Constants (cInteger)
import Ocram.Analysis.CallGraph (start_functions)
import Ocram.Transformation.Inline.Types
import Ocram.Transformation.Inline.Names
import Ocram.Transformation.Util (un, ident)

addMain :: Transformation
addMain cg (CTranslUnit decls ni) = return $ CTranslUnit (decls ++ extraDecls) ni
  where
    extraDecls = decl "pal_init" : decl "pal_run" : defMain : []
    decl name = CDeclExt (CDecl [CTypeSpec (CVoidType un)] [(Just (CDeclr (Just (ident name)) [CFunDeclr (Right ([], False)) [] un] Nothing [] un), Nothing, Nothing)] un)
    defMain = CFDefExt (CFunDef [CTypeSpec (CIntType un)] (CDeclr (Just (ident "main")) [CFunDeclr (Right ([], False)) [] un] Nothing [] un) [] body un)
    body = CCompound [] (map CBlockStmt (call "pal_init" : threads ++ [call "pal_run", return0])) un
    call name = CExpr (Just (CCall (CVar (ident name) un) [] un )) un 
    return0 = CReturn (Just (CConst (CIntConst (cInteger 0) un))) un 
    threads = map startThread [1..Set.size (start_functions cg)]
    startThread tid = CExpr (Just (CCall (CVar (ident (threadExecutionFunction tid)) un) [CVar (ident "NULL") un] un)) un
