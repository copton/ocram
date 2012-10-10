module Ocram.Backend
-- exports {{{1
(
  tcode_2_ecode
) where

-- imports {{{1
import Language.C.Data.Ident (internalIdent)
import Language.C.Data.Node (undefNode)
import Language.C.Syntax.AST
import Ocram.Analysis (Analysis(..))
import Ocram.Backend.EStack
import Ocram.Backend.ThreadExecutionFunction
import Ocram.Backend.TStack
import Ocram.Debug (CTranslUnit', VarMap')
import Ocram.Intermediate (Function(..), Variable(..))
import Ocram.Names (tframe)
import Ocram.Ruab (Variable(StaticVariable))
import Ocram.Symbols (Symbol, symbol)

import qualified Data.Map as M

tcode_2_ecode :: Analysis -> M.Map Symbol Function -> (CTranslUnit', CTranslUnit, VarMap') -- {{{1
tcode_2_ecode ana cfs =
  let
    (tframes, tstacks) = create_tstacks (anaCallgraph ana) (anaBlocking ana) cfs
    (eframes, estacks) = create_estacks (anaCallgraph ana) cfs
    bfds               = blockingFunctionDeclarations (anaBlocking ana)
    (stDecls, stVm)    = staticVariables cfs
    (tefs, vm)         = thread_execution_functions (anaCallgraph ana) (anaBlocking ana) cfs estacks

    decls              = anaNonCritical ana ++ map CDeclExt (map snd tframes ++ tstacks ++ eframes ++ bfds ++ stDecls)
    fundefs            = map CFDefExt tefs
    ecode              = CTranslUnit (decls ++ fundefs) undefNode

    bfframes           = M.elems $ M.fromList tframes `M.intersection` (anaBlocking ana)
    pal                = CTranslUnit (map CDeclExt (bfframes ++ bfds)) undefNode
  in
    (ecode, pal, stVm ++ vm)

blockingFunctionDeclarations :: M.Map Symbol CDecl -> [CDecl] -- {{{2
blockingFunctionDeclarations = map go . M.elems
  where
  go cd = decl
    where
      un = undefNode
      decl = CDecl ts [(Just declr, Nothing, Nothing)] un
      ts = [CTypeSpec (CVoidType un)]
      declr = CDeclr iden fdeclr Nothing [] un
      iden = Just (internalIdent fName) 
      fName = symbol cd 
      fdeclr = [CFunDeclr (Right ([param], False)) [] un]
      param = CDecl ts' [(Just declr', Nothing, Nothing)] un 
      ts' = [CTypeSpec (CTypeDef (internalIdent (tframe fName)) un)]
      declr' = CDeclr Nothing [CPtrDeclr [] un] Nothing [] un

staticVariables :: M.Map Symbol Function -> ([CDecl], VarMap') -- {{{2
staticVariables = M.fold update ([], [])
  where
    update fun (decls, vm) =
      let (decl', vm') = unzip $ map create (fun_stVars fun) in
      (concat decl' ++ decls, concat vm' ++ vm)

    create (EVariable _) = ([], [])
    create (TVariable tname decl scope) = ([decl], [(StaticVariable tname, scope, symbol decl)])
