module Ocram.Backend
-- exports {{{1
(
  tcode_2_ecode
) where

-- imports {{{1
import Data.Generics (everywhere, mkT, extT)
import Language.C.Data.Ident (internalIdent)
import Language.C.Data.Node (undefNode)
import Language.C.Syntax.AST
import Ocram.Analysis (Analysis(..))
import Ocram.Backend.EStack
import Ocram.Backend.ThreadExecutionFunction
import Ocram.Backend.TStack
import Ocram.Debug.Enriched (CTranslUnit', CExtDecl', CStat', CDecl', eun, aset, node_start, ENodeInfo(EnWrapper))
import Ocram.Debug.Types (VarMap')
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

    nonCrit            = map setBreakpoints (anaNonCritical ana)
    decls              = map (CDeclExt . aset eun) $ map snd tframes ++ tstacks ++ eframes ++ bfds ++ stDecls
    fundefs            = map CFDefExt tefs
    ecode              = CTranslUnit (nonCrit ++ decls ++ fundefs) eun

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

setBreakpoints :: CExtDecl  -> CExtDecl' -- {{{2
setBreakpoints = everywhere (mkT tStat `extT` tInitDecl) . fmap EnWrapper
  where
    tStat :: CStat' -> CStat'
    tStat o@(CCompound _ _ _) = o
    tStat o@(CExpr Nothing _) = o
    tStat o@(CLabel _ _ _ _) = o
    tStat s = amap node_start s

    tInitDecl :: CDecl' -> CDecl'
    tInitDecl (CDecl x1 decls x2) = CDecl x1 (map tr decls) x2
      where
        tr (y1, Just y2, y3) = (y1, Just (fmap node_start y2), y3)
        tr x = x
