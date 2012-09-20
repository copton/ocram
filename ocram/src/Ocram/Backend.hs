{-# LANGUAGE TemplateHaskell #-}
module Ocram.Backend
-- exports {{{1
(
  tcode_2_ecode
) where

-- imports {{{1
import Control.Applicative ((<$>), (<*>))
import Language.C.Syntax.AST
import Language.C.Data.Node (undefNode)
import Language.C.Data.Ident (internalIdent)
import Ocram.Analysis (Analysis(..))
import Ocram.Backend.TStack
import Ocram.Backend.EStack
import Ocram.Backend.ThreadExecutionFunction
import Ocram.Debug (enrich_node_info, CTranslUnit')
import Ocram.Intermediate (Function(..), Variable(..))
import Ocram.Names (tframe)
import Ocram.Symbols (Symbol, symbol)
import Ocram.Util (abort, unexp)

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

blockingFunctionDeclarations :: M.Map Symbol CDecl -> [CDecl]
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

staticVariables :: M.Map Symbol Function -> [CDecl]
staticVariables = map mkVar . M.fold (\fun vs -> fun_stVars fun ++ vs) []
  where
    mkVar = renameDecl <$> var_fqn <*> var_decl
    renameDecl newName (CDecl x1 [(Just declr, x2, x3)] x4) =
      CDecl x1 [(Just (renameDeclr newName declr), x2, x3)] x4
    renameDecl _ x = $abort $ unexp x

    renameDeclr newName (CDeclr (Just _) x1 x2 x3 x4) =
      CDeclr (Just (internalIdent newName)) x1 x2 x3 x4
    renameDeclr _ x = $abort $ unexp x
