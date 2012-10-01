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
import Ocram.Debug (enrich_node_info, CTranslUnit')
import Ocram.Intermediate (Function(..), Variable(..))
import Ocram.Names (tframe)
import Ocram.Symbols (Symbol, symbol)

import qualified Data.Map as M

tcode_2_ecode :: Analysis -> M.Map Symbol Function -> (CTranslUnit', CTranslUnit') -- {{{1
tcode_2_ecode ana cfs =
  let
    (tframes, tstacks) = create_tstacks (anaCallgraph ana) (anaBlocking ana) cfs
    (eframes, estacks) = create_estacks (anaCallgraph ana) cfs
    bfds               = blockingFunctionDeclarations (anaBlocking ana)
    stVars             = staticVariables cfs
    tefs               = thread_execution_functions (anaCallgraph ana) (anaBlocking ana) cfs estacks

    decls              = anaNonCritical ana ++ map CDeclExt (map snd tframes ++ tstacks ++ eframes ++ bfds ++ stVars)
    fundefs            = map CFDefExt tefs
    ecode              = CTranslUnit (decls ++ fundefs) undefNode

    bfframes           = M.elems $ M.fromList tframes `M.intersection` (anaBlocking ana)
    pal                = CTranslUnit (map CDeclExt (bfframes ++ bfds)) undefNode
  in
    (fmap enrich_node_info ecode, fmap enrich_node_info pal)

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

staticVariables :: M.Map Symbol Function -> [CDecl] -- {{{2
staticVariables = M.fold (\fun vs -> map var_decl (fun_stVars fun) ++ vs) []
